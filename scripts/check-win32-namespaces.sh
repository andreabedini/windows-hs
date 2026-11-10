#!/bin/sh
# Generate a module for every namespace in metadata/, compile each with
# -Werror, and check its struct layouts against the Windows headers with
# mingw-w64. This covers all of Win32, where check-win32-surface.sh covers
# the functions Hackage's Win32 package binds.
#
# Usage: scripts/check-win32-namespaces.sh
#
# With ONLY set to a list of namespaces separated by spaces, only those
# are checked: ONLY=Windows.Win32.UI.Shell scripts/check-win32-namespaces.sh
#
# All of Win32 in one module would be about 1.2 million lines, far too much
# to compile, so each namespace gets its own module with the types it
# depends on: about 2.8 million lines in all. Compiling them takes about
# 40 minutes the first time and a lot of memory, which is why this is not
# part of `cabal test all`. Modules of $LARGE lines or more are compiled one
# at a time, with GHC's heap limited to $LARGE_HEAP (the largest,
# Windows.Win32.Web.MsHtml, needs about 4 GB), and the others $JOBS at a
# time, each limited to $HEAP. Lower JOBS on a machine with little memory.
#
# Items the generator cannot express are reported but do not fail the
# check: variadic functions, which the Haskell FFI cannot call, and the few
# that use WinRT types, which are out of scope. Any other skipped item does.
#
# The layout checks are reported but do not fail the check either.
# mingw-w64's headers lack many types (those checks are counted as
# unchecked), and some of its headers differ from the Windows SDK's, which
# win32metadata is generated from: mingw's mpeg2structs.h packs fewer
# structs than the SDK does, for example. The mismatches are listed in
# layout-mismatches for review. The layout check is skipped when
# x86_64-w64-mingw32-gcc is not installed.
#
# Everything is kept in dist-newstyle/win32-namespaces; GHC recompiles only
# the modules whose source changed. Each namespace's compiler output is in
# logs/NAMESPACE.log there, and gcc's in logs/NAMESPACE.layout.log.
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
self="$root/scripts/check-win32-namespaces.sh"
work="$root/dist-newstyle/win32-namespaces"
JOBS=${JOBS:-4}
HEAP=${HEAP:-3g}
LARGE=${LARGE:-40000}
LARGE_HEAP=${LARGE_HEAP:-6g}

# The path of a namespace's module.
source_of() {
  printf '%s/src/%s.hs' "$work" "$(printf '%s' "$1" | tr . /)"
}

# Workers that xargs runs, one namespace each.
case ${1-} in
  --generate)
    src=$(source_of "$2")
    mkdir -p "$(dirname "$src")"
    "$BINDGEN" --in "$root/metadata" -m "$2" -o "$src" \
      --c-layout-checks "$work/layout/$2.c" "$2" 2>"$work/warnings/$2"
    exit
    ;;
  --compile)
    if ghc -O0 -Wall -Wcompat -Werror -XGHC2024 \
      -hide-all-packages -package base -package windows-core \
      -no-link -outputdir "$work/build" "$(source_of "$2")" \
      +RTS -M"$3" -RTS >"$work/logs/$2.log" 2>&1; then
      printf 'ok %s\n' "$2"
    else
      printf 'failed %s\n' "$2"
    fi
    exit
    ;;
  --compile-all)
    xargs -P 1 -I '{}' "$self" --compile '{}' "$LARGE_HEAP" <"$work/large"
    xargs -P "$JOBS" -I '{}' "$self" --compile '{}' "$HEAP" <"$work/small"
    exit
    ;;
  --layout)
    c="$work/layout/$2.c"
    "$GCC" -std=c11 -fsyntax-only "$c" >"$work/logs/$2.layout.log" 2>&1 || true
    awk -v c="$c" -f "$root/scripts/classify-layout-errors.awk" \
      "$c" "$work/logs/$2.layout.log" >"$work/logs/$2.layout"
    printf '%s %s\n' "$(head -n 1 "$work/logs/$2.layout")" "$2"
    exit
    ;;
esac

cd "$root"
cabal build -v0 windows-bindgen winmd windows-core
BINDGEN=$(cabal list-bin -v0 windows-bindgen)
export BINDGEN

rm -rf "$work/src" "$work/layout" "$work/warnings" "$work/logs"
mkdir -p "$work/src" "$work/layout" "$work/warnings" "$work/logs" "$work/build"

# winmd-dump with only a file prints each namespace and its number of types.
if [ -n "${ONLY-}" ]; then
  printf '%s\n' $ONLY >"$work/namespaces"
else
  for f in metadata/*.winmd; do
    cabal run -v0 winmd-dump -- "$f"
  done | awk 'NF == 2 { print $1 }' | sort -u >"$work/namespaces"
fi
total=$(wc -l <"$work/namespaces")

printf 'generating %d namespaces\n' "$total"
xargs -P "$JOBS" -n 1 "$self" --generate <"$work/namespaces"

cat "$work"/warnings/* | sort -u >"$work/skipped"
grep -v -e 'variadic functions are not supported' -e 'is a WinRT type' \
  "$work/skipped" >"$work/unexpected" || true

# Largest first, so the long compiles start early.
while read -r ns; do
  printf '%s %s\n' "$(wc -l <"$(source_of "$ns")")" "$ns"
done <"$work/namespaces" | sort -rn >"$work/sizes"
awk -v n="$LARGE" '$1 >= n { print $2 }' "$work/sizes" >"$work/large"
awk -v n="$LARGE" '$1 < n { print $2 }' "$work/sizes" >"$work/small"

printf 'compiling %d namespaces (%d lines)\n' "$total" \
  "$(awk '{ s += $1 } END { print s }' "$work/sizes")"
# cabal exec gives ghc, and the workers it runs, the project's packages.
cabal exec -v0 -- "$self" --compile-all >"$work/compiled"

if gcc=$(command -v x86_64-w64-mingw32-gcc); then
  GCC=$gcc
  export GCC
  printf 'checking layouts\n'
  xargs -P "$JOBS" -n 1 "$self" --layout <"$work/namespaces" >"$work/layouts"
  for ns in $(cat "$work/namespaces"); do
    tail -n +2 "$work/logs/$ns.layout"
  done | sort -u >"$work/layout-mismatches"
  awk '{ p += $1; u += $2; m += $3 } END {
      printf "layouts: %d assertions passed, %d unchecked, %d mismatched\n", p, u, m
    }' "$work/layouts"
  printf '%d distinct layout mismatches against mingw-w64 (see %s)\n' \
    "$(wc -l <"$work/layout-mismatches")" "$work/layout-mismatches"
else
  printf 'x86_64-w64-mingw32-gcc not found; layout checks skipped\n' >&2
fi

skipped=$(wc -l <"$work/skipped")
unexpected=$(wc -l <"$work/unexpected")
failed=$(grep -c '^failed' "$work/compiled" || true)
printf '%d namespaces: %d failed to compile\n' "$total" "$failed"
grep '^failed' "$work/compiled" | while read -r _ ns; do
  printf '  %s (see %s)\n' "$ns" "$work/logs/$ns.log"
done
printf '%d items skipped (variadic functions and WinRT types; see %s)\n' \
  "$((skipped - unexpected))" "$work/skipped"
if [ "$unexpected" -gt 0 ]; then
  printf '%d items skipped unexpectedly:\n' "$unexpected" >&2
  cat "$work/unexpected" >&2
fi
[ "$failed" -eq 0 ] && [ "$unexpected" -eq 0 ]

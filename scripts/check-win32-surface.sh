#!/bin/sh
# Generate bindings for every function in coverage/win32/names.txt, compile
# them with -Werror, and check their struct layouts against the Windows
# headers with mingw-w64.
#
# Usage: scripts/check-win32-surface.sh
#
# The generated module and a cabal project that builds it are kept in
# dist-newstyle/win32-surface, so later runs rebuild only what changed.
# Compiling the module takes about a minute and over 1 GB of memory, which
# is why this is not part of `cabal test all`. The layout check is skipped
# when x86_64-w64-mingw32-gcc is not installed.
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
work="$root/dist-newstyle/win32-surface"
mkdir -p "$work/src/Windows"

cd "$root"
cabal build -v0 windows-bindgen
cabal run -v0 windows-bindgen -- \
  -n coverage/win32/names.txt \
  -o "$work/src/Windows/Win32.hs" \
  --c-layout-checks "$work/layout-checks.c" \
  metadata/Windows.Win32.winmd 2>"$work/warnings"
if [ -s "$work/warnings" ]; then
  cat "$work/warnings" >&2
  printf 'windows-bindgen skipped some items\n' >&2
  exit 1
fi

cat >"$work/cabal.project" <<EOF
packages: . $root/windows-core
EOF
cat >"$work/win32-surface.cabal" <<'EOF'
cabal-version:    3.4
name:             win32-surface
version:          0
build-type:       Simple

library
  ghc-options:      -Wall -Wcompat -Werror
  default-language: GHC2024
  hs-source-dirs:   src
  exposed-modules:  Windows.Win32
  build-depends:    base, windows-core
EOF

(cd "$work" && cabal build -v1 win32-surface)

if gcc=$(command -v x86_64-w64-mingw32-gcc); then
  "$gcc" -std=c11 -fsyntax-only "$work/layout-checks.c"
else
  printf 'x86_64-w64-mingw32-gcc not found; layout checks skipped\n' >&2
fi

printf '%d functions generated, compiled and checked\n' \
  "$(grep -cv -e '^#' -e '^$' coverage/win32/names.txt)"

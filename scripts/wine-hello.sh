#!/bin/sh
# Build samples/hello with a Windows GHC running under wine, then run it.
#
# Usage: scripts/wine-hello.sh GHC_DIR [ARG]...
#
# GHC_DIR is an unpacked Windows GHC binary distribution (the directory
# containing bin/ghc.exe), e.g. from
# https://downloads.haskell.org/~ghc/9.12.2/ghc-9.12.2-x86_64-unknown-mingw32.tar.xz
# ARGs are passed to hello.exe; use --no-gui to skip the message box.
#
# The wine prefix defaults to dist-wine/prefix in the repository; set
# WINEPREFIX to use another one.
set -eu

if [ $# -lt 1 ]; then
  printf 'usage: %s GHC_DIR [ARG]...\n' "$0" >&2
  exit 2
fi

ghc_dir=$1
shift

root=$(cd "$(dirname "$0")/.." && pwd)
out="$root/dist-wine"
mkdir -p "$out"

: "${WINEPREFIX:=$out/prefix}"
: "${WINEDEBUG:=-all}"
export WINEPREFIX WINEDEBUG

cd "$root"
# The packages set default-language: GHC2024 in their .cabal files; ghc.exe
# is called directly here, so pass it explicitly.
wine "$ghc_dir/bin/ghc.exe" \
  -XGHC2024 \
  -iwindows-core/src -isamples/hello/src -isamples/hello/app \
  -outputdir dist-wine/build \
  -o dist-wine/hello.exe \
  samples/hello/app/Main.hs

wine dist-wine/hello.exe "$@"

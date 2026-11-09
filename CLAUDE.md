# CLAUDE.md

windows-hs generates Haskell bindings to the Windows API from Microsoft's
win32metadata, modelled on Rust's windows-rs. Read `README.md` for the
architecture and status.

## Settled decisions

Do not reopen these without the maintainer.

- Generator first: the main deliverable is the generator plus a small
  runtime. Users generate only the APIs they need. Pre-generated Hackage
  packages come later.
- Dynamic loading: generated code resolves every function with
  `LoadLibraryExW` + `GetProcAddress`, caches the pointer, and calls it
  through `foreign import ccall "dynamic"`. Never generate static
  `foreign import ccall` bindings to Win32 functions and never require import
  libraries. Only the runtime's own bootstrap (`LoadLibraryExW`,
  `GetProcAddress`, `GetLastError`, `FreeLibrary`) is statically imported from
  kernel32.
- Independent of the `Win32` package: do not depend on or reuse types from
  Hackage's `Win32` package.
- Native metadata reader: `winmd` parses `.winmd` files directly. Do not use
  win32json or any .NET tooling.

Scope: x86_64 Windows only (stdcall and ccall are the same there). Win32 and
COM only; WinRT is out of scope.

## Layout

Planned Cabal packages, one directory each, listed in `cabal.project`:

- `winmd/`: ECMA-335 reader library. No Windows-specific code. Must build and
  test on Linux.
- `windows-bindgen/`: generator library and `windows-bindgen` executable.
- `windows-core/`: runtime library that generated code imports.

Update this section when a package is added or its role changes.

## Metadata

`./scripts/fetch-metadata.sh` downloads the pinned NuGet package and extracts
`metadata/Windows.Win32.winmd`. `metadata/` is git-ignored: the files are under
the Microsoft SDK licence, so never commit them. To move to a new metadata
release, change `version` and `sha256` in the script together.

Tests that need the metadata take its path from the `WINMD_PATH` environment
variable, defaulting to `metadata/Windows.Win32.winmd` relative to the
repository root.

## Building and testing

- Toolchain: GHC 9.12 and cabal 3.18 from ghcup.
- `cabal build all`, `cabal test all`.
- Development happens on Linux. The reader and generator are portable Haskell
  and are tested on Linux. Generated bindings and `windows-core` type-check on
  Linux, but running them needs Windows. There is no GHC Windows cross-compiler
  here, so runtime tests run on Windows CI.
- `x86_64-w64-mingw32-gcc` and `wine` are installed. Use them to check
  generated struct layouts: emit a C file of `sizeof`/`offsetof` checks against
  `windows.h`, compile it with mingw, and run it under wine.

## Conventions

- Dependencies for `winmd`: boot packages only (`base`, `bytestring`, `text`,
  `containers`, `array`). Ask before adding anything else.
- Metadata accessors are total over well-formed files. Validate table and heap
  bounds once when a file is loaded, and report malformed input with an error
  that names the table, row and column.
- Follow windows-rs (`windows-bindgen`) for questions about how metadata
  should be interpreted. Cite the ECMA-335 partition and section (e.g.
  II.22.37) in comments where the format is subtle.
- Version control is jj. Commit messages use Conventional Commits.

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

Cabal packages, one directory each, listed in `cabal.project`:

- `winmd/`: ECMA-335 reader library. No Windows-specific code. Must build and
  test on Linux.
  - `WinMD.Internal.Parser`: bounds-checked little-endian parser.
  - `WinMD.Internal.PE`: finds the metadata in the PE image.
  - `WinMD.Internal.Database`: table schema, coded indexes, loading and
    validation, raw column and heap access.
  - `WinMD.Tables`: typed rows (`Row 'TTypeDef`, aliased `TypeDef`) and
    accessors; `WinMD.Tables.Flags` has flag constants.
  - `WinMD.Signature`, `WinMD.Attribute`: blob decoders.
  - `winmd-dump` (app/Dump.hs): prints namespaces, types, or one type in a
    C#-like form. Use it to look at metadata before writing code against it.
- `windows-bindgen/` (planned): generator library and `windows-bindgen`
  executable.
- `windows-core/` (planned): runtime library that generated code imports.

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
- `loadDatabase` checks every heap and table reference in every row, so column
  and heap accessors read without bounds checks. Its errors name the table,
  row and column. Blob decoders (signatures, constants, attributes) come in an
  `Either` form and a throwing form (`WinMDError`) whose message names the row.
- win32metadata quirks to keep in mind: a type name can have several
  definitions, one per architecture (`findTypeDefs`, `SupportedArchitecture`);
  nested types have an empty namespace and are referenced by TypeRefs scoped
  to the enclosing TypeRef (`resolveTypeRef`).
- Tests in `winmd/test` run against the real `Windows.Win32.winmd`. Assert
  concrete values checked against the Windows headers or docs, and keep the
  test that decodes every signature, constant and attribute in the file.
- Follow windows-rs (`windows-bindgen`) for questions about how metadata
  should be interpreted. Cite the ECMA-335 partition and section (e.g.
  II.22.37) in comments where the format is subtle.
- Version control is jj. Commit messages use Conventional Commits.

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
- `windows-bindgen/`: generator library and `windows-bindgen` executable.
  - `Windows.Bindgen.Model`: classifies type definitions (`Kind`), picks the
    x64 definition of architecture-specific types, expands requested names
    into a `Selection` with every type it depends on.
  - `Windows.Bindgen.Layout`: x64 sizes, alignments and field offsets, the
    way MSVC lays out structs (packing from ClassLayout, explicit offsets
    from FieldLayout).
  - `Windows.Bindgen.Names`: metadata names to Haskell identifiers.
  - `Windows.Bindgen.Emit`: writes the module. A type that cannot be emitted
    is dropped together with everything that refers to it, and reported as
    a warning, so the output always compiles.
  - `Windows.Bindgen.cLayoutChecks`: C `_Static_assert`s for the layouts of
    the emitted structs.
- `windows-core/`: runtime library that generated code imports (as
  `Windows.Core`): `resolveProc` (LoadLibraryExW/GetProcAddress with a module
  cache), struct-by-value marshalling for the x64 ABI, `FixedArray`, `GUID`,
  errors (`HRESULT`, the `WindowsError` exception, `lastError`, `check`),
  union members (`Member`, `getMember`, `setMember`, `zeroed`), and strings
  (`WideString`, with `OsString` and `Maybe OsString` instances, for
  parameters; `withWideBuffer`, `peekWideBuffer` and `takeWideString` for
  strings Windows writes; depends on `os-string`). Its few static kernel32
  imports are stubs that throw on other systems, so the library builds,
  links and loads on Linux
  (tests, GHCi, Template Haskell such as `[osstr|...|]`); only calling
  Windows needs Windows. Its test suite covers the parts that call no
  Windows functions (`HRESULT`, `Member`, `WideString`).
- `samples/hello/`: a module generated from `bindgen-names.txt` and a small
  program using it. The library (bindings and `Hello`) builds everywhere; the
  executable only on Windows.
- `coverage/win32/names.txt`: the win32metadata name of every function
  Hackage's Win32 package imports, the first coverage milestone. Written by
  `scripts/win32-surface-names.sh`; do not edit by hand.

Update this section when a package is added or its role changes.

## Generated code

- One module per run, containing the requested items and their dependency
  closure. Generated code uses only qualified imports for functions
  (`P.`, `S.`, `F.`, `U.`, `Core.`) so Win32 names cannot clash with them.
- Functions: `proc'name` holds the resolved pointer (a NOINLINE CAF),
  `call'name` is the `dynamic` import, `name` is the exported wrapper. Calls
  are `safe`.
- Wrappers follow windows-rs 0.58, the last release generated from
  win32metadata (`functionShape` in `Emit`): an `HRESULT` is checked; a
  `BOOL` or a handle from a function marked `SetLastError` throws
  `lastError` on failure; a trailing out-only parameter of at most 16 bytes
  is returned. `CanReturnMultipleSuccessValues` functions and everything
  else keep their C result. `--sys` (`cfgSys`) keeps every C signature, like
  windows-sys. Current windows-rs reads its own metadata, so check rules
  against 0.58 (`crates/libs/bindgen/src/metadata.rs`, `rust/functions.rs`).
- String parameters (`[In] [Const] PWSTR`, NUL-terminated: not
  `NativeArrayInfo`, `MemorySize`, `NotNullTerminated` or
  `NullNullTerminated`) take any `Core.WideString s => s`: an `OsString`
  or `OsPath`, `Maybe OsString` (`Nothing` is NULL) where optional, or the
  module's `PWSTR`, which keeps string constants and `MAKEINTRESOURCE`
  values (`IDC_ARROW`) usable; the metadata does not mark which parameters
  accept those. Generated modules define `instance Core.WideString PWSTR`.
  `PSTR` (ANSI, code-page dependent) stays raw. Not under `--sys`.
- Strings Windows writes are returned as `Core.OsString` (not under
  `--sys`):
  - An `[Out] PWSTR` buffer sized by an `[In]` integer parameter
    (`NativeArrayInfo(CountParamIndex)`, used by no other array) is not an
    argument: the wrapper allocates a zeroed buffer of that many UTF-16
    units and returns its contents up to the first NUL, after the C result
    (`getCurrentDirectoryW :: Word32 -> IO (Word32, Core.OsString)`). What a
    function does when the buffer is too small differs and is not in the
    metadata, so retrying is left to the caller. Buffers sized through a
    pointer (`GetComputerNameW`) stay raw, as in windows-rs 0.58.
  - A returned out parameter (see above) of type `PWSTR*` marked
    `FreeWith(f)`, where `f` takes one pointer, is read and freed with `f`,
    also when the call fails (`SHGetKnownFolderPath`). `select` adds every
    function a `FreeWith` names to the selection. Almost nothing in the
    metadata is marked `FreeWith`, so other strings Windows allocates stay
    `Ptr PWSTR`.
- Structs passed or returned by value follow the x64 rule: 1, 2, 4 or 8 bytes
  as an integer, otherwise by pointer (`Windows.Core.Marshal`).
- Constants are pattern synonyms of every kind windows-sys has: numbers,
  enum and typedef values, pointers given as integers (`HKEY_LOCAL_MACHINE`,
  `IDC_ARROW`, matched through `F.ptrToIntPtr`), GUIDs (`Guid` attribute),
  struct initializers (`Constant` attribute, e.g. `PROPERTYKEY`), and
  strings. A string constant is `PWSTR`, or `PSTR` when marked
  `NativeEncoding("ansi")`; it points to a `NOINLINE` value allocated on
  first use by `Core.wideLiteral`/`Core.ansiLiteral`, and matching it
  compares pointers, as C does.
- `HRESULT` comes from windows-core (`coreType` in `Model`), as in
  windows-rs, so that error codes from any generated module and from
  `WindowsError` are the same type. Generated modules re-export it.
- Reading `GetLastError` after a generated call is reliable: calls are
  `safe`, and GHC's runtime saves and restores the last error around them
  and across thread switches (`rts/Schedule.c`).
- Unions and explicit-layout structs are newtypes over their bytes. Each
  member gets a `Core.Member` instance (its offset) and an `R.HasField`
  instance, so it reads like a field (`info.anonymous.wProcessorArchitecture`);
  values are built with `Core.setMember @"name" x Core.zeroed`. Member types
  are part of the dependency closure.
- Struct records use `DuplicateRecordFields` and `NoFieldSelectors`; users
  read fields with `OverloadedRecordDot`.
- `samples/hello/src/Windows/Win32.hs` is a golden file. After changing the
  generator, regenerate it with the command in `bindgen-names.txt` and review
  the diff; the test suite fails when it is stale.

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
  and are tested on Linux. Generated bindings and `windows-core` compile on
  Linux, but running them needs Windows.
- `scripts/wine-hello.sh GHC_DIR [--no-gui]` builds `samples/hello` with a
  Windows GHC binary distribution under wine and runs it.
- `cabal test all` checks that every name in `coverage/win32/names.txt`
  generates without skips. `scripts/check-win32-surface.sh` also compiles
  the result with `-Werror` (about a minute, over 1 GB) and checks its
  layouts with mingw; run it after changing the generator.
- To check generated struct layouts by hand, emit a C file of
  `sizeof`/`offsetof` checks against `windows.h`, compile it with
  `x86_64-w64-mingw32-gcc`, and run it under wine.

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
- Commit messages use Conventional Commits.

# windows-hs

[![CI](https://github.com/andreabedini/windows-hs/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/andreabedini/windows-hs/actions/workflows/ci.yml)

Native Windows support for Haskell, generated from the same API description
Microsoft uses for its own language projections.

Microsoft publishes the Windows API as machine-readable metadata,
[win32metadata](https://github.com/microsoft/win32metadata), and generates
Rust's [windows-rs](https://github.com/microsoft/windows-rs) crates from it.
windows-hs takes the same approach for Haskell: a metadata reader, a
generator that turns the APIs you name into a Haskell module, and a small
runtime library that the generated code uses.

> **Status:** early development, not yet on Hackage. Functions, constants,
> enums, structs, unions and callbacks generate, compile, and have the struct
> layouts of the Windows headers for every function Hackage's `Win32` package
> binds. The sample program runs on Windows 11 with GHC 9.12. COM interfaces
> are opaque types without methods, and WinRT is out of scope.

## Why

Haskell programs mostly reach Windows through the
[`Win32`](https://hackage.haskell.org/package/Win32) package, whose bindings
are written by hand. It binds 544 of the roughly 15,000 functions in the
metadata, and each binding is a static `foreign import`, so a function can be
linked only if mingw-w64 ships an import library for its DLL. Anything else
has to be bound by hand, with the C header open and the types, struct layouts
and error conventions worked out each time.

The metadata already records what those bindings need, often more than the C
headers carry: which DLL exports each function, the layout of every struct,
which parameters are optional, output-only, or the size of another, how a
function reports failure, and which function frees what it returns. Generating
from it gives the whole API, consistently, and a newer metadata release brings
newer APIs without anyone writing bindings.

## How it works

| Package | Role | windows-rs equivalent |
|---|---|---|
| `winmd` | Reads `.winmd` files (ECMA-335, the .NET assembly format): tables, signatures, custom attributes | `windows-metadata` |
| `windows-bindgen` | Generates a Haskell module with the functions, constants and types you name, and everything they depend on | `windows-bindgen` |
| `windows-core` | Runtime for generated code: loading functions, errors, strings, structs passed by value | `windows-core`, `windows-link` |

Design choices:

- **Generate what you use.** The whole API is too large to compile as one
  Haskell package. You name the functions, types or namespaces you need and
  add the generated module to your project. Pre-generated packages may come
  later.
- **Functions are loaded at runtime.** Generated code finds each function
  with `LoadLibraryExW` and `GetProcAddress` on its first call and caches the
  pointer, so no import libraries are needed. A missing DLL or export is
  reported when the function is first called.
- **Wrappers follow windows-rs.** Where the metadata says how a function
  fails, the wrapper checks it and throws `Windows.Core.WindowsError`.
  UTF-16 strings are `OsString`s. With `--sys`, every function keeps its C
  signature instead, like windows-sys.
- **Independent of `Win32`.** Every type is generated from the metadata.
- **No .NET tooling.** `winmd` reads the binary format directly.

windows-hs targets x86_64 Windows and covers Win32 and COM. The metadata
reader and the generator are portable Haskell, developed on Linux, where
generated modules also compile; running them needs Windows.

## Quick look

[`samples/hello`](samples/hello) generates its bindings from
[`bindgen-names.txt`](samples/hello/bindgen-names.txt) and calls them, for
example:

```haskell
-- HRESULT is checked; the path Windows allocates is returned and freed.
documents <- with FOLDERID_Documents $ \folder -> shGetKnownFolderPath folder 0 (HANDLE nullPtr)

-- String parameters take an OsString (or OsPath); optional ones take Nothing.
answer <- messageBoxW (HWND nullPtr) [osstr|Hello from Haskell!|] (Just [osstr|windows-hs|]) MB_OKCANCEL

-- Constants are pattern synonyms; IDC_ARROW is a resource number passed as a string.
HCURSOR cursor <- loadCursorW (HINSTANCE nullPtr) IDC_ARROW

-- CloseHandle returns BOOL and sets the last error, so its wrapper throws.
closed <- try @WindowsError (closeHandle (HANDLE nullPtr))
```

On Windows, `cabal run hello` runs it.

## Using it

### Fetch the metadata

The `.winmd` files are covered by the Microsoft SDK licence and are not stored
in this repository. To download the pinned version
(`Microsoft.Windows.SDK.Win32Metadata` 71.0.45-preview) and check its
SHA-256:

```sh
./scripts/fetch-metadata.sh
```

This writes `metadata/Windows.Win32.winmd`. It needs `curl` and `unzip`.

### Generate a module

Name the functions, constants and types you need, or whole namespaces.
`windows-bindgen` writes one module with those items and every type they
depend on:

```sh
cabal run -v0 windows-bindgen -- -o src/Windows/Win32.hs \
  metadata/Windows.Win32.winmd \
  Windows.Win32.System.SystemInformation.GetTickCount \
  Windows.Win32.UI.WindowsAndMessaging.MessageBoxW
```

Each name is a win32metadata namespace followed by the item's name:

| To generate | Name |
|---|---|
| a function | `Windows.Win32.System.SystemInformation.GetTickCount` |
| a constant | `Windows.Win32.Foundation.MAX_PATH` |
| a type | `Windows.Win32.Foundation.RECT` |
| everything in a namespace | `Windows.Win32.System.SystemInformation` |

Names can also be listed in a file, one per line (`#` starts a comment), and
passed with `-n FILE`. A name that matches nothing stops the generator. See
`windows-bindgen --help` for the other options.

win32metadata's namespaces do not follow the C headers: `GetTickCount` is
declared in `sysinfoapi.h` but lives in
`Windows.Win32.System.SystemInformation`. To find an item's namespace, search
the [windows-rs documentation](https://microsoft.github.io/windows-docs-rs/),
which is generated from the same metadata, and replace `::` with `.` in its
path. `winmd-dump` (below) lists the functions in a namespace you can guess.
Looking up an item by its short name is planned.

### Depend on windows-core

The generated module imports `Windows.Core`. Until the packages are on
Hackage, add `windows-core` to your `cabal.project` from this repository:

```cabal
source-repository-package
  type:     git
  location: https://github.com/andreabedini/windows-hs
  tag:      <commit>
  subdir:   windows-core
```

and `windows-core` to your package's `build-depends`, with `os-string` if
you write `[osstr|...|]` literals.

### What the generated code looks like

- **Errors.** As in windows-rs, a function returning `HRESULT` is checked,
  and one returning `BOOL` or a handle and setting the last error throws on
  failure. A trailing output parameter is returned rather than passed.
  Everything else keeps its C result.
- **String parameters** that a function only reads take any
  `Core.WideString`: an `OsString` or `OsPath`, which on Windows already
  holds the UTF-16 the W functions expect, including names that are not valid
  Unicode; `Nothing` where the parameter is optional; or the module's `PWSTR`,
  for string constants and resource numbers such as `IDC_ARROW`. Write
  literals as `[osstr|...|]`.
- **Strings Windows writes** come back as `OsString`. When a function writes
  to a buffer whose size is another parameter, the wrapper takes the size,
  allocates the buffer, and returns what was written with the C result.
  Functions differ in what they do when the buffer is too small, and the
  metadata does not say, so asking again with a larger size is up to you:

  ```haskell
  (needed, _) <- getCurrentDirectoryW 0           -- the size it needs
  (_, directory) <- getCurrentDirectoryW needed
  ```

  A string that Windows allocates is read and freed for you where the
  metadata names the function that frees it, as for `SHGetKnownFolderPath`.
- **Structs** are records, read with `OverloadedRecordDot`. Unions read the
  same way (`info.anonymous.wProcessorArchitecture`) and are built with
  `Core.setMember`.
- **Constants** are pattern synonyms: numbers, enum values, GUIDs, strings,
  and handles given as integers (`HKEY_LOCAL_MACHINE`).

### Look at the metadata

`winmd-dump` prints what the metadata says about a namespace or a type:

```console
$ cabal run -v0 winmd-dump -- metadata/Windows.Win32.winmd Windows.Win32.Foundation RECT
[Documentation("https://learn.microsoft.com/windows/win32/api/windef/ns-windef-rect")]
struct Windows.Win32.Foundation.RECT : System.ValueType
  int left
  int top
  int right
  int bottom
```

With only the file it lists every namespace; with a namespace, its types;
`winmd-dump FILE NAMESPACE Apis` shows the namespace's functions and
constants.

## Development

```sh
./scripts/fetch-metadata.sh
cabal build all
cabal test all
```

The tests read `metadata/Windows.Win32.winmd`; set `WINMD_PATH` to use a
different file. Development happens on Linux with GHC 9.12.

[`coverage/win32/names.txt`](coverage/win32/names.txt) lists every function
the `Win32` package binds, the first coverage target; the tests check that
they all generate. `scripts/check-win32-surface.sh` also compiles them with
`-Werror` and checks their struct layouts against the Windows headers with
`x86_64-w64-mingw32-gcc`. Run it after changing the generator.

To run the sample without Windows, unpack a
[Windows GHC binary distribution](https://downloads.haskell.org/~ghc/9.12.2/)
and run it under wine:

```sh
scripts/wine-hello.sh path/to/ghc-9.12.2-x86_64-unknown-mingw32 --no-gui
```

## Licence

Licensed under either of the [MIT licence](LICENSE-MIT) or the
[Apache License, Version 2.0](LICENSE-APACHE), at your option, as windows-rs
is. The Windows metadata is not part of this repository and is under its own
licence.

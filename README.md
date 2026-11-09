# windows-hs

Haskell bindings to the Windows API, generated from Microsoft's
[win32metadata](https://github.com/microsoft/win32metadata). It is modelled on
Rust's [windows-rs](https://github.com/microsoft/windows-rs): a machine-readable
description of the API surface goes in, and Haskell bindings come out.

> **Status:** early development. The metadata reader (`winmd`) works and is
> tested against `Windows.Win32.winmd`. The generator and runtime are not
> written yet, so there are no bindings to use.

## How it works

Microsoft publishes the Win32 API as ECMA-335 metadata (`.winmd` files, the
same format .NET assemblies use). Each function, struct, enum, constant,
callback and COM interface is described there, along with details that C
headers do not carry: which DLL exports a function, which parameters are
optional or output-only, how an array's length is given, and which function
frees a handle.

windows-hs has three parts:

| Package | Role | windows-rs equivalent |
|---|---|---|
| `winmd` | Reads `.winmd` files: PE container, metadata tables, signatures, custom attributes | `windows-metadata` |
| `windows-bindgen` (planned) | Generates Haskell modules for the APIs you select | `windows-bindgen` |
| `windows-core` (planned) | Small runtime that generated code depends on: function loading, `HRESULT`, strings, COM plumbing | `windows-core`, `windows-link` |

### Design choices

- **Generate what you use.** All of Win32 is around 20,000 functions across
  hundreds of namespaces, too much to compile as one Haskell package. You run
  the generator on the namespaces or functions you need and add its output to
  your project. Ready-made packages on Hackage may come later.
- **Functions are loaded at runtime.** Generated code finds each function with
  `LoadLibraryExW` and `GetProcAddress` the first time it is called, and caches
  the pointer. No import libraries are needed, so APIs that mingw-w64 does not
  ship import libraries for work as well as any other. A missing DLL or export
  is reported when the function is first called.
- **Independent of the `Win32` package.** Every type is generated from metadata.
  windows-hs does not depend on, or share types with,
  [`Win32`](https://hackage.haskell.org/package/Win32).
- **A native metadata reader.** `winmd` reads the binary format directly, with
  no .NET tooling or third-party JSON conversion in between.

### Scope

- Targets x86_64 Windows, the platform GHC supports.
- Covers Win32 and COM. WinRT uses the same file format but needs much more
  runtime support (generics, async, activation), so it is out of scope for now.

## Fetching the metadata

The `.winmd` files are covered by the Microsoft SDK licence and are not stored
in this repository. To download the pinned version
(`Microsoft.Windows.SDK.Win32Metadata` 71.0.45-preview) and check its SHA-256:

```sh
./scripts/fetch-metadata.sh
```

This writes `metadata/Windows.Win32.winmd`. It needs `curl` and `unzip`.

## Looking at the metadata

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

With only the file argument it lists every namespace and how many types it
has. With a namespace it lists the types in it.

## Building and testing

```sh
./scripts/fetch-metadata.sh
cabal build all
cabal test all
```

The tests read `metadata/Windows.Win32.winmd`; set `WINMD_PATH` to use a
different file.

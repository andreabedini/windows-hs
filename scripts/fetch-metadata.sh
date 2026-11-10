#!/bin/sh
# Download the pinned win32metadata NuGet package and extract the .winmd
# files into metadata/: Windows.Win32.winmd, and Microsoft.Direct3D.D3D12.winmd,
# which defines the Direct3D 12 types that Windows.Win32.winmd uses. The package is licensed under the Windows SDK licence
# (sdk_license.txt in the package), so the files are fetched on demand rather
# than committed.
set -eu

version=71.0.45-preview
sha256=c26f8ac7190dcd5599d074b534c5e2c1860cca84e107a63090ac22aceb7ad556

root=$(cd "$(dirname "$0")/.." && pwd)
dest="$root/metadata"
pkg="$dest/microsoft.windows.sdk.win32metadata.$version.nupkg"
url="https://api.nuget.org/v3-flatcontainer/microsoft.windows.sdk.win32metadata/$version/microsoft.windows.sdk.win32metadata.$version.nupkg"

mkdir -p "$dest"

if [ ! -f "$pkg" ]; then
  curl --fail --silent --show-error --location --output "$pkg.part" "$url"
  mv "$pkg.part" "$pkg"
fi

if command -v sha256sum >/dev/null 2>&1; then
  actual=$(sha256sum "$pkg" | cut -d' ' -f1)
else
  actual=$(shasum -a 256 "$pkg" | cut -d' ' -f1)
fi

if [ "$actual" != "$sha256" ]; then
  printf 'error: %s has SHA-256 %s, expected %s\n' "$pkg" "$actual" "$sha256" >&2
  exit 1
fi

unzip -o -q "$pkg" 'Windows.Win32.winmd' 'Microsoft.Direct3D.D3D12.winmd' 'sdk_license.txt' -d "$dest"
printf '%s\n' "$dest/Windows.Win32.winmd" "$dest/Microsoft.Direct3D.D3D12.winmd"

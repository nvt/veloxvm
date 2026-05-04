#!/bin/sh
#
# Bake a compiled .vm app into vm-app-image.h so the firmware ships
# with a RAM-resident program ready to autorun.
#
# Strict mode: any failed step aborts the script. Without it a failed
# compile would leave a stale or empty header behind and the next
# firmware build would silently flash broken bytecode.

set -eu

target=vm-app-image.h

if [ $# -eq 0 ]; then
  echo "Usage: $0 <category/name>" >&2
  exit 1
fi

app=$1
name=$(basename "$app")
dir=$(dirname "$app")
script_dir=$(cd "$(dirname "$0")" && pwd)
repo_root=$(cd "$script_dir/../.." && pwd)
out="$script_dir/$target"
bytecode="$repo_root/apps/$dir/bin/$name.vm"

(
  cd "$repo_root"
  ./compile.sh "$app"
)

if [ ! -f "$bytecode" ]; then
  echo "Compiled bytecode not found: $bytecode" >&2
  exit 1
fi

echo "Creating $app in $target"
rm -f "$out"
"$repo_root/tools/create-ram-module" "$bytecode" > "$out"
echo "const char vm_program_name[] = \"$name.vm\";" >> "$out"

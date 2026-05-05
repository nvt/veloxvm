#!/bin/sh
#
# Bake a compiled .vm app into vm-app-image.h so the firmware ships
# with a RAM-resident program ready to autorun.
#
# This is a thin wrapper around the Makefile: it just sets VM_APP and
# asks make to (re)generate the image. The Makefile takes care of the
# compile.sh + create-ram-module steps. The script is kept for
# backwards compatibility and as a "do this one thing" helper -- the
# canonical flow is `make TARGET=zoul VM_APP=<app>`.

set -eu

if [ $# -eq 0 ]; then
  echo "Usage: $0 <category/name>" >&2
  exit 1
fi

script_dir=$(cd "$(dirname "$0")" && pwd)
exec make -C "$script_dir" VM_APP="$1" vm-app-image.h

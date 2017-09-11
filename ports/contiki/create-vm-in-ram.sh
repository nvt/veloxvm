#!/bin/sh

target=vm-app-image.h

if [ $# -eq 0 ]; then
  echo "No program specified"
  exit 1
fi

(cd ../../ && ./compile.sh $1 && echo Creating $1 in $target && rm -f ports/contiki/$target && tools/create-ram-module apps/$1.vm >ports/contiki/$target)
echo "const char vm_program_name[] = \"$1.vm\";" >> $target

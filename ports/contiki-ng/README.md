This directory contains a port of VeloxVM to the Contiki-NG, an operating
system for the IoT available at https://github.com/contiki-ng/contiki-ng.
Contiki-NG is a fork of the Contiki operating system, focusing on standard
and state-of-the-art IoT protocols.

The required version of Contiki-NG to run with this port is Contiki-NG 4.0.

To use this port, the variable CONTIKI in the Makefile should point to your
local Contiki-NG directory. The default setting is with a relative path,
assuming that Contiki-NG is placed in the same directory as Velox.

  CONTIKI = ../../../contiki-ng

## Baking a VM application into the firmware

The firmware ships with one VeloxVM application baked in as a
RAM-resident bytecode image (`vm-app-image.h`). The Makefile generates
this header automatically. Pick the application via `VM_APP`, in the
form `<category>/<name>` (the same form that `compile.sh` accepts):

  make TARGET=zoul VM_APP=basic/ticker
  make TARGET=zoul VM_APP=algorithms/sieve

`VM_APP` defaults to `basic/ticker`. The image is regenerated whenever
the underlying source/bytecode changes; `compile.sh` is the
authoritative incrementality checker.

The legacy helper `./create-vm-in-ram.sh <app>` is kept and now just
forwards to the Makefile, so existing scripts keep working.

`vm-app-image.h` is a generated artefact and is excluded from version
control via the local `.gitignore`.

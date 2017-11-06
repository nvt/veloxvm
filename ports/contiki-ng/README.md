This directory contains a port of VeloxVM to the Contiki-NG, an operating
system for the IoT available at https://github.com/contiki-ng/contiki-ng.
Contiki-NG is a fork of the Contiki operating system, focusing on standard
and state-of-the-art IoT protocols.

The required version of Contiki-NG to run with this port is Contiki-NG 4.0.

To use this port, the variable CONTIKI in the Makefile should point to your
local Contiki-NG directory. The default setting is with a relative path,
assuming that Contiki-NG is placed in the same directory as Velox.

  CONTIKI = ../../../contiki-ng


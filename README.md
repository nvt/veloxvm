## Overview

VeloxVM is a virtual machine for resource-constrained devices in the
Internet of Things. Its primary purpose is to provide a safe and
versatile execution environment for small applications on such
devices. To this end, VeloxVM features high-level programming
languages, preemptive multithreading, exception handling, resource
provisioning, and security policies. The services that many IoT
operating systems are unable to give to applications because of a lack
of hardware and language support can thus be provided by VeloxVM
instead. Despite providing a comprehensive feature set, VeloxVM is
able to execute together with a host operating system on
resource-constrained IoT devices with as little as 32 kB RAM and 256
kB ROM.

### Application Development

Applications can be written in the _Scheme_ programming language or
_Cyclus_, which is a new language for IoT application scripting
developed in conjunction with VeloxVM. Most of the [Scheme standard
version R5RS](http://www.schemers.org/Documents/Standards/R5RS/) is
supported, along with some extensions. Additionally, a considerable
number of procedures have been added for network programming and
accessing the typical services of an IoT operating system. Cyclus is
basically an imperative script language with an infix syntax. When
compiling Cyclus scripts, they get translated to Scheme as an
intermediary step.

The applications are compiled to a custom bytecode format with
high-level instructions. The instruction set contains 208
instructions, and is designed to make IoT apps efficient to express in
bytecode. A compact bytecode makes software updates fast and
energy-efficient to send over radio, and to store on the
devices---either in RAM or in ROM.

### Supported operating systems

* POSIX systems such as Linux, *BSD, macOS, and Windows with Cygwin
  (ports/posix)
* [Contiki-NG](https://www.contiki-ng.org/) (ports/contiki-ng)

## Docker setup

The easiest way to get started with VeloxVM is to use a ready-made Docker
image available from Docker Cloud. This image contains all of the required
software, a compiled VM binary, and compiled example scripts.

For instance, to try the prime number sieve example, run Docker as follows:

  <code>docker run nvt1/veloxvm:latest bin/vm apps/sieve.vm</code>

This command will fetch a Docker image that has been generated from the
latest pull request in the nvt/veloxvm repository at GitHub. The image will
then be cached locally for subsequent runs.

Another possibility is to go into a bash shell, and navigate directly within
the Docker container:

  <code>docker run -it nvt1/veloxvm:latest bash</code>

## Manual setup

Before using the VM for the first time, please ensure that you have the
following software installed:
 * bison
 * flex
 * clang (or gcc)
 * [Racket](https://racket-lang.org/) 8.0 or later — required by the
   Scheme front-end compiler under `languages/scheme-racket/`.

To be able to run all the tests, the following software is also needed:
 * afl-fuzz
 * python3
 * python3-pexpect

### Archived Common Lisp compiler

The original Scheme compiler was written in Common Lisp and is preserved
under `languages/scheme-cl-legacy/` for historical reference only. It emits
bytecode format version 1 and works only with a VeloxVM built at master
commit `6f3c0a3` (2026-04-20, the last v1 commit) or earlier. It is not
maintained and is not part of the active build. See that directory's
README for details.

## Trying out VeloxVM in a POSIX environment

1. Build the VM and all tools.

  <code>make</code>

2. Build the VM apps.

  <code>./compile.sh [&lt;app name&gt;]</code>

   If no parameter is supplied to the compile.sh script, all apps will
   be compiled. The parameter "app name" refers to the name of a app
   in the "apps" directory. The name of the app should be without the
   suffix in the filename; e.g., <code>./compile.sh math</code> to
   compile a file named either math.scm or math.cyl in the apps
   directory.

3. Run an app.

  <code>bin/vm &lt;app path&gt;</code>

   E.g., <code>bin/vm apps/math.vm</code>

## Trying out VeloxVM in Contiki-NG

1. Enter the port directory.

  <code>cd ports/contiki-ng</code>

2. Generate a RAM image of the bytecode for an app, in this case "math".

  <code>./create-vm-in-ram.sh math</code>

3. Build a Contiki-NG firmware with Velox included.

  This step requires that we select an appropriate OS platform to build the
  VM for. We will use the Zoul platform, because it has sufficient RAM and ROM
  for a basic Velox installation.

  <code>make TARGET=zoul vm.upload login</code>

## Additional Reading Material

1. N. Tsiftes and T. Voigt. [Velox VM: A safe execution environment for
resource-constrained IoT applications](https://doi.org/10.1016/j.jnca.2018.06.001).
<i>Journal of Network and Computer Applications</i>, Volume 118, pages 61-73. 2018.

2. N. Tsiftes. [Storage-Centric System Architectures for Networked,
Resource-Constrained Devices](http://www.diva-portal.org/smash/record.jsf?pid=diva2%3A882135).
<i>Digital Comprehensive Summaries of Uppsala Dissertations from the Faculty of Science and
Technology</i> 1331. Uppsala: Acta Universitatis Upsaliensis. 2016.


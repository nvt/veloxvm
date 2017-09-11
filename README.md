VeloxVM is a virtual machine for resource-constrained devices in the
Internet of Things. Its primary purpose is to provide a safe and versatile
execution environment for small applications on such devices. To this end,
VeloxVM features high-level programming languages, a compact bytecode format,
preemptive multithreading, exception handling, resource provisioning,
and security policies.

Before using the VM for the first time, please ensure that you have the
following software installed:
 * bison
 * flex
 * clisp (or one of the other LISP packages listed below)

To try out VeloxVM in a POSIX environment, do the following:

1. Build the VM and all tools.

  > make

2. Build the VM apps.

   This step requires either GNU CLISP (clisp), Steel Bank Common LISP (sbcl),
   CMU Common LISP (cmucl), Armed Bear Common LISP (abcl), or Clozure CL (ccl)
   to be installed.

   By default, the system uses GNU CLISP, but this can be changed
   in compiler/run.sh by setting the CL_IMPL parameter to either of the values
   enclosed within parentheses above.

   In addition, native compiler executables are supported. To use such an
   executable, the CL_IMPL value should be set to "native". At the moment,
   there is only support for creating a native compiler using SBCL. It should
   also be possible, however, to create a native compiler manually using
   another LISP implementation and naming the resulting file "compiler".

   To create a native compiler based on SBCL, go to compiler/ and 
   run "sbcl --load sbcl-compile.lisp".

   Once the compiler setup is finished, VM applications can be programmed
   by invoking the following command.

  > ./compile.sh [<app name>]

   If no parameter is supplied to the compile.sh script, all apps will
   be compiled. The parameter "app name" refers to the name of a app
   in the "apps" directory. The name of the app should be without the
   suffix in the filename; e.g., ./compile.sh math

3. Run an app.

  > bin/vm <app path>

   E.g., bin/vm apps/math.vm

To try out VeloxVM on the Zoul platform for Contiki OS, do the following:

1. Enter the Contiki port directory.

  > cd ports/contiki

2. Generate a RAM image of the bytecode for an app, in this case "math".

  > ./create-vm-in-ram.sh math

3. Build a Contiki firmware with the VM linked with it and loaded as a
   Contiki process.

  > make TARGET=zoul vm.upload login

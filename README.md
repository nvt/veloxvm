# VeloxVM

## Overview

VeloxVM is a safe, portable execution environment for applications on
resource-constrained Internet of Things (IoT) devices. It runs alongside a
host operating system and provides facilities often unavailable on small
devices, including high-level languages, preemptive multithreading, exception
handling, resource provisioning, and security policies. VeloxVM can operate
on devices with as little as 32 kB RAM and 256 kB ROM.

### Application development

Applications can be written in Scheme or Python. The Scheme frontend supports
most of [R5RS](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/)
with extensions for networking and common IoT operating-system services. The
Python frontend supports a practical subset of Python and compiles it to the
same runtime format.

Both frontends produce compact VeloxVM bytecode. The 208-instruction format
reduces the storage and radio-transfer costs of deploying applications to
constrained devices.

### Supported operating systems

- POSIX systems such as Linux, BSD, macOS, and Windows with Cygwin
  (`ports/posix`)
- [Contiki-NG](https://www.contiki-ng.org/) (`ports/contiki-ng`)

## Quick start

Install the build dependencies listed below, then build the VM and compile an
example:

```sh
make
./compile.sh basic/factorial
./run.sh basic/factorial
```

Run `./compile.sh` without an application name to compile all applications and
benchmarks. Build products are written to a `bin/` directory beside their
source category; for example, `apps/basic/factorial.scm` becomes
`apps/basic/bin/factorial.vm`.

## Docker

A prebuilt Linux/AMD64 image is published on
[Docker Hub](https://hub.docker.com/r/nvt1/veloxvm):

```sh
docker pull nvt1/veloxvm:latest
docker run --rm nvt1/veloxvm:latest \
  bin/vm apps/basic/bin/factorial.vm
```

The image contains the VM, compiler toolchains, repository sources, and
compiled example applications. To explore the image interactively, start a
shell:

```sh
docker run --rm -it nvt1/veloxvm:latest bash
```

To build the image locally from the repository instead:

```sh
docker build -f tools/docker/Dockerfile -t veloxvm .
```

## Prerequisites

To build the VM and its tools, install:

- Bison
- Flex
- Clang or GCC
- Make
- [Racket](https://racket-lang.org/) 8.0 or later (for the Scheme compiler)
- Python 3 (for the Python compiler and test utilities)

Some test suites have additional dependencies. See [Testing](#testing).

## Building and running applications

Applications may be written in Scheme (`.scm`) or Python (`.py`). The compiler
selects the frontend from the file extension.

```sh
./compile.sh                         # compile all apps and benchmarks
./compile.sh algorithms/sieve.py     # compile one source file
./compile.sh -f basic/factorial      # force recompilation

./run.sh algorithms/sieve            # locate and run compiled bytecode
bin/vm apps/algorithms/bin/sieve.vm  # invoke the VM directly
```

See [`apps/README.md`](apps/README.md) for the example layout and
[`doc/scheme.md`](doc/scheme.md) or [`doc/python.md`](doc/python.md) for
language-specific guidance.

## Testing

Test suites are grouped by purpose and can be run independently:

```sh
tests/unit-tests/run-tests.sh
tests/primitives/run-tests.sh
tests/python-tests/run-tests.sh
tests/multi-app/run-tests.sh
```

Functional tests require Python 3 and Pexpect. Fuzzing additionally requires
American Fuzzy Lop (`afl-fuzz`). See [`tests/README.md`](tests/README.md).

## Contiki-NG

The Contiki-NG port has separate setup and firmware instructions in
[`ports/contiki-ng/README.md`](ports/contiki-ng/README.md).

## Documentation

The main guides and references are:

- [Scheme guide](doc/scheme.md) and [R5RS compliance](doc/scheme-r5rs-compliance.md)
- [Python frontend](doc/python.md)
- [Primitive table](doc/primitives.md) and [instruction-set reference](doc/instruction-set.md)
- [Bytecode format](doc/bytecode-format.md)
- [Interactive REPL](tools/repl/README.md)

### Archived Common Lisp compiler

The original Scheme compiler is preserved under
`languages/scheme-cl-legacy/` for historical reference. It emits bytecode
format version 1 and only works with VeloxVM at commit `6f3c0a3` (the last v1
commit) or earlier. It is not part of the active build; see that directory's
README for details.

## Additional reading

1. N. Tsiftes and T. Voigt. [Velox VM: A safe execution environment for
resource-constrained IoT applications](https://doi.org/10.1016/j.jnca.2018.06.001).
<i>Journal of Network and Computer Applications</i>, Volume 118, pages 61-73. 2018.

2. N. Tsiftes. [Storage-Centric System Architectures for Networked,
Resource-Constrained Devices](http://www.diva-portal.org/smash/record.jsf?pid=diva2%3A882135).
<i>Digital Comprehensive Summaries of Uppsala Dissertations from the Faculty of Science and
Technology</i> 1331. Uppsala: Acta Universitatis Upsaliensis. 2016.

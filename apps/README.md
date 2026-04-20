# VeloxVM example applications

Source trees are organised by theme. Each source file compiles to a `.vm`
bytecode file under a sibling `bin/` directory.

```
apps/basic/        Small programs exercising core language features
apps/algorithms/   Classic algorithms, often in more than one language
apps/networking/   UDP clients and servers, DNS, packet manipulation
apps/embedded/     Contiki/Contiki-NG examples (LEDs, sensors, RPL, ...)
```

Every example is in Scheme, Cyclus, or Python. The file extension
(`.scm`, `.cyl`, `.py`) selects the front-end; the same VM runs all three.

## Building and running

```sh
./compile.sh                           # build everything that needs it
./compile.sh basic/factorial           # build one example (any language)
./compile.sh algorithms/sieve.py       # build one specific source file
./compile.sh -f                        # force a rebuild

./run.sh basic/factorial               # find and run bin/factorial.vm
bin/vm apps/basic/bin/factorial.vm     # or just invoke the VM directly
```

`compile.sh` skips files whose `.vm` output is newer than both the source
and the `bin/vm` binary; `--force` disables the skip.

This port compiles VeloxVM into a large Javascript file.

To use it, first install Emscripten from
http://kripken.github.io/emscripten-site

Once the repository is downloaded, you need to set up the path to the
Emscripten wrappers for Make and the C compiler. This is typically done
by running the following command in the Emscripten directory:

  source ./emsdk_set_env.sh

You can then build the Javascript file by running the following command:

  emmake make

If the build succeeds the Javascript file's relative path will be bin/vm.js.

It can be executed in a Unix environment by using the node.js tool:

  nodejs bin/vm.js <path to VM app>

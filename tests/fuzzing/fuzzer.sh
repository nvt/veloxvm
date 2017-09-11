#!/bin/sh

VM_PATH=../../

echo Preparing the execution environment for the fuzz tool.
sleep 1

# Set compiler and core dumping method so that it is usable by afl-fuzz.
if [ "$(uname)" = "Darwin" ]; then
    CC=afl-clang
    launchctl unload -w /System/Library/LaunchAgents/com.apple.ReportCrash.plist
    sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.ReportCrash.Root.plist
elif [ "$(uname)" = "Linux" ]; then
    CC=afl-gcc
    sudo bash -c "echo core >/proc/sys/kernel/core_pattern"
fi

make -C $VM_PATH CC=$CC \
  DEFINES+="-DVM_HEAP_SIZE=10000000 -DVM_OBJECT_POOL_SIZE=10000000" clean vm

command -v afl-fuzz > /dev/null 2>&1 || \
  { echo "afl-fuzz must be installed to run this script" >&2; exit 1; }

afl-fuzz -i test-cases -o results -- $VM_PATH/bin/vm @@

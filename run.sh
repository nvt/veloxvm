#!/usr/bin/env bash

# Purpose: Convenient script to run VeloxVM applications from repository root
#
# Usage: ./run.sh <path> [args...]
#
# The <path> can be:
#   - Category/app name (e.g., "basic/factorial", "algorithms/sieve")
#   - Just app name (searches all directories, e.g., "factorial", "sieve")
#   - Full path to .vm file
#
# Examples:
#   ./run.sh basic/factorial
#   ./run.sh algorithms/sieve
#   ./run.sh sieve                    # Finds first match
#   ./run.sh deep-structures          # From benchmarks/

# Directories to search for compiled apps
BINDIRS=("apps" "benchmarks")

# find_vm_file: Find a compiled .vm file by name
find_vm_file() {
  local NAME=$1

  # Strip .vm extension if provided
  NAME=${NAME%.vm}

  # Try exact path first
  if [ -f "$NAME.vm" ]; then
    echo "$NAME.vm"
    return 0
  fi

  if [ -f "$NAME" ] && [[ "$NAME" == *.vm ]]; then
    echo "$NAME"
    return 0
  fi

  # Try in bin/ directories
  for DIR in "${BINDIRS[@]}"; do
    # Try direct path: DIR/NAME/bin/$(basename NAME).vm
    # e.g., apps/basic/bin/hello.vm for NAME=basic/hello
    if [[ "$NAME" == */* ]]; then
      # NAME contains a path component
      BASENAME=$(basename "$NAME")
      if [ -f "$DIR/$NAME/bin/$BASENAME.vm" ]; then
        echo "$DIR/$NAME/bin/$BASENAME.vm"
        return 0
      fi
    fi

    # Try flat path in bin/
    if [ -f "$DIR/bin/$NAME.vm" ]; then
      echo "$DIR/bin/$NAME.vm"
      return 0
    fi

    # Search recursively in bin/ subdirectories
    FOUND=$(find "$DIR" -path "*/bin/$NAME.vm" -o -path "*/bin/$(basename $NAME).vm" 2>/dev/null | head -1)
    if [ -n "$FOUND" ]; then
      echo "$FOUND"
      return 0
    fi
  done

  return 1
}

# Main script

if [ $# -eq 0 ]; then
  echo "Usage: $0 <app> [args...]"
  echo ""
  echo "Available applications:"
  echo ""

  # List available apps by category
  for DIR in "${BINDIRS[@]}"; do
    if [ -d "$DIR" ]; then
      echo "From $DIR/:"
      find "$DIR" -name "*.vm" -type f 2>/dev/null | \
        sed "s|$DIR/bin/||; s|$DIR/||; s|\.vm$||" | \
        sort | sed 's/^/  /'
      echo ""
    fi
  done

  echo "Examples:"
  echo "  $0 basic/factorial"
  echo "  $0 algorithms/sieve"
  echo "  $0 factorial              # Searches all directories"
  exit 1
fi

APP_NAME=$1
shift  # Remove first argument, rest are passed to the VM

# Find the .vm file
VM_FILE=$(find_vm_file "$APP_NAME")

if [ -z "$VM_FILE" ] || [ ! -f "$VM_FILE" ]; then
  echo "Error: Cannot find compiled app: $APP_NAME"
  echo ""
  echo "Try compiling it first:"
  echo "  ./compile.sh $APP_NAME"
  echo ""
  echo "Or list available apps:"
  echo "  $0"
  exit 1
fi

# Check if VM binary exists
if [ ! -f "bin/vm" ]; then
  echo "Error: VM binary not found at bin/vm"
  echo "Please run 'make' first to build the VM"
  exit 1
fi

# Run the app
echo "Running: $VM_FILE"
echo ""
bin/vm "$VM_FILE" "$@"

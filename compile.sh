#!/usr/bin/env bash

# Purpose: Compilation script for Velox applications.
#
# Usage: ./compile.sh [-f|--force] [<path>]
#
# Options:
#   -f, --force     Force recompilation even if files are up-to-date
#
# When no path is supplied, this script builds all available apps, tests, and benchmarks.
#
# When the optional path argument is supplied, this script compiles a single app/test/benchmark.
# The path can be:
#   - Relative to apps/, tests/apps/, or benchmarks/ (e.g., "basic/factorial", "sieve")
#   - A full path (e.g., "apps/basic/factorial.scm")
#   - Just the name without extension (searches all directories)
#
# Examples:
#   ./compile.sh                    # Compile everything (incremental)
#   ./compile.sh -f                 # Force recompile everything
#   ./compile.sh basic/factorial    # Compile apps/basic/factorial.scm
#   ./compile.sh -f sieve           # Force recompile sieve
#   ./compile.sh algorithms/sieve.py  # Compile specific Python file

# Directories to search for source files
APPDIRS=("apps" "benchmarks")

# VM binary path for timestamp checking
VM_BINARY="bin/vm"

# Force compilation flag
FORCE_COMPILE=0

# needs_compilation: Check if source file needs to be recompiled
# Returns 0 (true) if compilation needed, 1 (false) if up-to-date
needs_compilation() {
  local SOURCE_FILE=$1
  local OUTPUT_FILE=$2

  # If force flag is set, always compile
  if [ $FORCE_COMPILE -eq 1 ]; then
    return 0
  fi

  # If output doesn't exist, need to compile
  if [ ! -f "$OUTPUT_FILE" ]; then
    return 0
  fi

  # If source is newer than output, need to recompile
  if [ "$SOURCE_FILE" -nt "$OUTPUT_FILE" ]; then
    return 0
  fi

  # If VM binary is newer than output, need to recompile
  # (VM was rebuilt, bytecode format may have changed)
  if [ -f "$VM_BINARY" ] && [ "$VM_BINARY" -nt "$OUTPUT_FILE" ]; then
    return 0
  fi

  # Output is up-to-date
  return 1
}

# compile_scheme: Compile source files implemented in Scheme to Velox bytecode.
compile_scheme () {
  RACKET_COMPILER=./languages/scheme-racket/main.rkt

  SOURCES=$@

  # Check if racket is installed
  if ! command -v racket &> /dev/null; then
    echo "Error: Racket is not installed"
    echo "Please install Racket from https://racket-lang.org/"
    echo ""
    echo "To use the legacy Common Lisp compiler instead:"
    echo "  cd languages/scheme-cl-legacy && ./run.sh <files>"
    exit 1
  fi

  # Count files to compile and skip
  TO_COMPILE=0
  SKIPPED=0
  ERRORS=0

  # First pass: check which files need compilation
  declare -a FILES_TO_COMPILE
  for source_file in ${SOURCES}; do
    SOURCE_DIR=$(dirname "$source_file")
    BIN_DIR="${SOURCE_DIR}/bin"
    BASENAME=$(basename "${source_file%.*}")
    OUTPUT_FILE="${BIN_DIR}/${BASENAME}.vm"

    if needs_compilation "$source_file" "$OUTPUT_FILE"; then
      FILES_TO_COMPILE+=("$source_file")
      TO_COMPILE=$((TO_COMPILE + 1))
    else
      SKIPPED=$((SKIPPED + 1))
    fi
  done

  # Print summary
  if [ $TO_COMPILE -gt 0 ]; then
    STR="files"
    if [ $TO_COMPILE -eq 1 ]; then
      STR="file"
    fi
    printf "Compiling %d Scheme %s using Racket compiler... " $TO_COMPILE $STR
  fi

  # Second pass: compile files that need it
  for source_file in "${FILES_TO_COMPILE[@]}"; do
    SOURCE_DIR=$(dirname "$source_file")
    BIN_DIR="${SOURCE_DIR}/bin"
    BASENAME=$(basename "${source_file%.*}")
    OUTPUT_FILE="${BIN_DIR}/${BASENAME}.vm"
    mkdir -p "$BIN_DIR"

    racket "$RACKET_COMPILER" -o "$OUTPUT_FILE" "$source_file" 2>&1 | grep -v "^Compiling\|^Compilation successful"
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
      ERRORS=$((ERRORS + 1))
    fi
  done

  # Print results
  if [ $TO_COMPILE -gt 0 ]; then
    if [ $ERRORS -eq 0 ]; then
      printf "OK\n"
    else
      printf "ERROR\n"
      return 1
    fi
  fi

  if [ $SKIPPED -gt 0 ]; then
    printf "Skipped %d up-to-date Scheme %s\n" $SKIPPED "$([ $SKIPPED -eq 1 ] && echo file || echo files)"
  fi
}

# compile_python: Compile Python source files to Velox bytecode.
compile_python() {
  PYTHON_COMPILER=./languages/python/pyvelox-compile
  SOURCES=$@

  if [ ! -x "$PYTHON_COMPILER" ]; then
    echo "Error: Python compiler not found at $PYTHON_COMPILER"
    return 1
  fi

  # Count files to compile and skip
  TO_COMPILE=0
  SKIPPED=0
  ERRORS=0

  # First pass: check which files need compilation
  declare -a FILES_TO_COMPILE
  for source_file in ${SOURCES}; do
    SOURCE_DIR=$(dirname "$source_file")
    BIN_DIR="${SOURCE_DIR}/bin"
    OUTPUT_FILE="${BIN_DIR}/$(basename ${source_file%.py}).vm"

    if needs_compilation "$source_file" "$OUTPUT_FILE"; then
      FILES_TO_COMPILE+=("$source_file")
      TO_COMPILE=$((TO_COMPILE + 1))
    else
      SKIPPED=$((SKIPPED + 1))
    fi
  done

  # Print summary
  if [ $TO_COMPILE -gt 0 ]; then
    STR="files"
    if [ $TO_COMPILE -eq 1 ]; then
      STR="file"
    fi
    printf "Compiling %d Python %s... " $TO_COMPILE $STR
  fi

  # Second pass: compile files that need it
  for source_file in "${FILES_TO_COMPILE[@]}"; do
    SOURCE_DIR=$(dirname "$source_file")
    BIN_DIR="${SOURCE_DIR}/bin"
    OUTPUT_FILE="${BIN_DIR}/$(basename ${source_file%.py}).vm"
    mkdir -p "$BIN_DIR"

    "$PYTHON_COMPILER" -o "$OUTPUT_FILE" "$source_file" 2>&1 | grep -v "^Compiling"
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
      ERRORS=$((ERRORS + 1))
    fi
  done

  # Print results
  if [ $TO_COMPILE -gt 0 ]; then
    if [ $ERRORS -eq 0 ]; then
      printf "OK\n"
    else
      printf "ERROR\n"
      return 1
    fi
  fi

  if [ $SKIPPED -gt 0 ]; then
    printf "Skipped %d up-to-date Python %s\n" $SKIPPED "$([ $SKIPPED -eq 1 ] && echo file || echo files)"
  fi
}

# translate_cyclus: Translate source files implemented in Cyclus to
#                   corresponding Scheme source code.
translate_cyclus() {
  CYCLUS_COMPILER=../cyclus/cyclus
  SOURCES=$@

  if [ ! -f $CYCLUS_COMPILER ]; then
    echo "Error: The Cyclus compiler is missing. Run \"make\" first."
    exit 1
  fi

  # Count files to translate and skip
  TO_TRANSLATE=0
  SKIPPED=0
  ERROR_FILES=()

  # First pass: check which files need translation
  declare -a FILES_TO_TRANSLATE
  for cyclus_file in $SOURCES
  do
    scheme_file="${cyclus_file%.*}.iscm"

    # Check if translation is needed (comparing .cyl to .iscm)
    if [ ! -f "$scheme_file" ] || [ "$cyclus_file" -nt "$scheme_file" ]; then
      FILES_TO_TRANSLATE+=("$cyclus_file")
      TO_TRANSLATE=$((TO_TRANSLATE + 1))
    else
      SKIPPED=$((SKIPPED + 1))
    fi
  done

  # Print summary
  if [ $TO_TRANSLATE -gt 0 ]; then
    STR="files"
    if [ $TO_TRANSLATE -eq 1 ]; then
      STR="file"
    fi
    printf "Translating %d Cyclus %s... " $TO_TRANSLATE $STR
  fi

  # Second pass: translate files that need it
  for cyclus_file in "${FILES_TO_TRANSLATE[@]}"
  do
    scheme_file="${cyclus_file%.*}.iscm"

    if [ $TO_TRANSLATE -gt 1 ]; then
      # Bulk mode: if multiple files are translated in the same invocation of
      # this script, we disable output to standard error to reduce noise.
      $CYCLUS_COMPILER $cyclus_file 1> $scheme_file 2> /dev/null
    else
      # Single file mode: show all output to help debugging.
      $CYCLUS_COMPILER $cyclus_file > $scheme_file
    fi

    # Remove the output file if the compilation failed.
    if [ $? -ne 0 ]; then
      rm -f $scheme_file
      ERROR_FILES+=($cyclus_file)
    fi
  done

  ERRORS=${#ERROR_FILES[@]}
  if [ $TO_TRANSLATE -gt 0 ]; then
    if [ $ERRORS -ne 0 ]; then
      printf "%d ERRORS:\n" $ERRORS
      for cyclus_file in "${ERROR_FILES[@]}"; do
        echo $cyclus_file
      done
    else
      printf "OK\n"
    fi
  fi

  if [ $SKIPPED -gt 0 ]; then
    printf "Skipped %d up-to-date Cyclus %s\n" $SKIPPED "$([ $SKIPPED -eq 1 ] && echo file || echo files)"
  fi
}

# find_source_file: Find a source file by name in standard directories
find_source_file() {
  local NAME=$1
  local BASENAME="${NAME%.*}"
  local EXT="${NAME##*.}"

  # Try exact path first
  if [ -f "$NAME" ]; then
    echo "$NAME"
    return 0
  fi

  # Check if extension was provided
  if [ "$EXT" = "scm" ] || [ "$EXT" = "iscm" ] || [ "$EXT" = "cyl" ] || [ "$EXT" = "py" ]; then
    # Extension provided, search for exact match
    for DIR in "${APPDIRS[@]}"; do
      if [ -f "$DIR/$NAME" ]; then
        echo "$DIR/$NAME"
        return 0
      fi
      # Try recursively
      FOUND=$(find "$DIR" -name "$NAME" -type f 2>/dev/null | head -1)
      if [ -n "$FOUND" ]; then
        echo "$FOUND"
        return 0
      fi
    done
  else
    # No extension provided, try all extensions
    for DIR in "${APPDIRS[@]}"; do
      for EXT in scm iscm cyl py; do
        if [ -f "$DIR/$NAME.$EXT" ]; then
          echo "$DIR/$NAME.$EXT"
          return 0
        fi
      done

      # Search recursively
      FOUND=$(find "$DIR" -name "$NAME.scm" -o -name "$NAME.iscm" -o -name "$NAME.cyl" -o -name "$NAME.py" 2>/dev/null | head -1)
      if [ -n "$FOUND" ]; then
        echo "$FOUND"
        return 0
      fi
    done
  fi

  return 1
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -f|--force)
      FORCE_COMPILE=1
      shift
      ;;
    -*)
      echo "Unknown option: $1"
      echo "Usage: ./compile.sh [-f|--force] [<path>]"
      exit 1
      ;;
    *)
      # This is the path argument, save and break
      TARGET="$1"
      shift
      break
      ;;
  esac
done

# Main body of this script.

if [ -z "$TARGET" ]; then
  # Compile all apps, tests, and benchmarks.
  echo "Compiling all applications, tests, and benchmarks..."
  echo ""

  # Translate Cyclus files
  cd languages/cyclus 2>/dev/null || true
  CYCLUS_FILES=""
  for DIR in "${APPDIRS[@]}"; do
    CYCLUS_FILES="$CYCLUS_FILES $(find ../../$DIR -name "*.cyl" 2>/dev/null)"
  done
  if [ -n "$CYCLUS_FILES" ]; then
    translate_cyclus $CYCLUS_FILES || true
  fi
  cd ../..

  # Compile Scheme files
  SCHEME_FILES=""
  for DIR in "${APPDIRS[@]}"; do
    SCHEME_FILES="$SCHEME_FILES $(find $DIR -name "*.scm" -o -name "*.iscm" 2>/dev/null)"
  done
  if [ -n "$SCHEME_FILES" ]; then
    compile_scheme $SCHEME_FILES || true
  fi

  # Compile Python files
  PYTHON_FILES=""
  for DIR in "${APPDIRS[@]}"; do
    PYTHON_FILES="$PYTHON_FILES $(find $DIR -name "*.py" 2>/dev/null)"
  done
  if [ -n "$PYTHON_FILES" ]; then
    compile_python $PYTHON_FILES || true
  fi
else
  # Compile a single, specified file.
  # Find the source file
  SOURCE_FILE=$(find_source_file "$TARGET")

  if [ -z "$SOURCE_FILE" ]; then
    echo "Error: Cannot find source file for: $TARGET"
    echo ""
    echo "Tried:"
    for DIR in "${APPDIRS[@]}"; do
      echo "  $DIR/$TARGET.{scm,iscm,cyl,py}"
    done
    exit 1
  fi

  echo "Found: $SOURCE_FILE"

  # Determine file type and compile
  case "$SOURCE_FILE" in
    *.cyl)
      cd languages/cyclus
      translate_cyclus "../../$SOURCE_FILE"
      cd ../..
      ISCM_FILE="${SOURCE_FILE%.*}.iscm"
      if [ -f "$ISCM_FILE" ]; then
        compile_scheme "$ISCM_FILE"
      fi
      ;;
    *.scm|*.iscm)
      compile_scheme "$SOURCE_FILE"
      ;;
    *.py)
      compile_python "$SOURCE_FILE"
      ;;
    *)
      echo "Error: Unknown file type: $SOURCE_FILE"
      exit 1
      ;;
  esac
fi

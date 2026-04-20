#!/usr/bin/env bash

# Purpose: Compilation script for Velox applications.
#
# Usage. ./compile.sh [<app name>]
#
# When no argument is supplied, this script builds all available apps.
#
# When the optional argument ("app name") is supplied, this script
# compiles a single app. The "app name" denotes the name of a file
# in the "app" directory, without any suffix.
#
# E.g., /compile.sh math

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

  STR="files"
  if [ $# -eq 1 ]; then
    STR="file"
  fi
  printf "Compiling %d Scheme %s using Racket compiler... " $# $STR

  # Compile each file individually
  ERRORS=0
  for source_file in ${SOURCES}; do
    racket "$RACKET_COMPILER" "$source_file" 2>&1 | grep -v "^Compiling\|^Compilation successful"
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
      ERRORS=$((ERRORS + 1))
    fi
  done

  if [ $ERRORS -eq 0 ]; then
    printf "OK\n"
  else
    printf "ERROR\n"
    return 1
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

  STR="files"
  if [ $# -eq 1 ]; then
    STR="file"
  fi
  printf "Translating %d Cyclus %s... " $# $STR
  ERROR_FILES=()

  for cyclus_file in $SOURCES
  do
    scheme_file="${cyclus_file%.*}.iscm"

    if [ $# -gt 1 ]; then
      # Bulk mode: if multiple files are translated in the same invocation of
      # this script, we disable output to standard error to reduce noise.
      $CYCLUS_COMPILER $cyclus_file 1> $scheme_file 2> /dev/null
    else
      # Single file mode: show all output to help debugging.
      $CYCLUS_COMPILER $cyclus_file > $scheme_file
    fi

    # Remove the output file if the compilation failed.
    if [ $? -ne 0 ]; then
      rm $scheme_file
      ERROR_FILES+=($cyclus_file)
    fi
  done

  ERRORS=${#ERROR_FILES[@]}
  if [ $ERRORS -ne 0 ]; then
    printf "%d ERRORS:\n" $ERRORS
    for cyclus_file in $ERROR_FILES; do
      echo $cyclus_file
    done

  else
    printf "OK\n"
  fi
}

# Main body of this script.

# Fetch the Velox application directory from the environment.
APPDIR=$VELOX_APPDIR

# Set the application directory to the default value if it is unset.
if [ -z $APPDIR ]; then
  APPDIR="./apps"
else
  echo "Using configured app directory" $APPDIR
fi

# Note: No longer changing to languages/scheme directory
# The Racket compiler is invoked directly from the root directory

if [ $# -eq 0 ]; then
  # Compile all apps.
  cd languages/cyclus 2>/dev/null || true
  if [ -d "../../apps" ]; then
    translate_cyclus `ls ../../apps/*.cyl 2>/dev/null` || true
  fi
  cd ../..

  # Compile both intermediate Scheme files (.iscm) and
  # regular Scheme files (.scm).
  compile_scheme `ls ${APPDIR}/*.{is,s}cm 2>/dev/null` || true
else
  # Compile a single, specified app.

  # Check if a Cyclus file exists, and compile it to Scheme in that case.
  FILE=${APPDIR}/$1.cyl
  if [ -e $FILE ]; then
    cd languages/cyclus
    translate_cyclus ../../$FILE
    cd ../..
  fi

  # Compile the Scheme source code of the app.
  SCM_FILE=${APPDIR}/$1.scm
  ISCM_FILE=${APPDIR}/$1.iscm

  if [ -e $SCM_FILE ]; then
    compile_scheme $SCM_FILE
  elif [ -e $ISCM_FILE ]; then
    compile_scheme $ISCM_FILE
  else
    echo "Error: Neither $SCM_FILE nor $ISCM_FILE exists."
  fi
fi

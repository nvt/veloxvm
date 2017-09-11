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
  COMPILE_SCRIPT=./run.sh

  SOURCES=$@

  STR="files"
  if [ $# -eq 1 ]; then
    STR="file"
  fi
  printf "Compiling %d Scheme %s... " $# $STR

  ($COMPILE_SCRIPT ${SOURCES})
  if [ $? -eq 0 ]; then
    printf "OK\n"
  else
    printf "ERROR\n"
  fi
}

# translate_Iota: Translate source files implemented in Iota to
#                 corresponding Scheme source code.
translate_Iota() {
  IOTA_COMPILER=../iota/iota
  SOURCES=$@

  STR="files"
  if [ $# -eq 1 ]; then
    STR="file"
  fi
  printf "Translating %d Iota %s... " $# $STR
  ERROR_FILES=()

  for iota_file in $SOURCES
  do
    scheme_file="${iota_file%.*}.iscm"

    if [ $# -gt 1 ]; then
      # Bulk mode: if multiple files are translated in the same invocation of
      # this script, we disable output to standard error to reduce noise.
      $IOTA_COMPILER $iota_file 1> $scheme_file 2> /dev/null
    else
      # Single file mode: show all output to help debugging.
      $IOTA_COMPILER $iota_file > $scheme_file
    fi

    # Remove the output file if the compilation failed.
    if [ $? -ne 0 ]; then
      rm $scheme_file
      ERROR_FILES+=($iota_file)
    fi
  done

  ERRORS=${#ERROR_FILES[@]}
  if [ $ERRORS -ne 0 ]; then
    printf "%d ERRORS:\n" $ERRORS
    for Iota_file in $ERROR_FILES; do
      echo $iota_file
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
  APPDIR="../../apps"
else
  echo "Using configured app directory" $APPDIR
fi

cd languages/scheme

if [ $# -eq 0 ]; then
  # Compile all apps.
  translate_Iota `ls ${APPDIR}/*.iota`
  # Compile both intermediate Scheme files (.iscm) and
  # regular Scheme files (.scm).
  compile_scheme `ls ${APPDIR}/*.{is,s}cm`
else
  # Compile a single, specified app.

  # Check if a Iota file exists, and compile it to Scheme in that case.
  FILE=${APPDIR}/$1.Iota
  if [ -e $FILE ]; then
    translate_Iota $FILE
  fi

  # Compile the Scheme source code of the app.
  FILE=${APPDIR}/$1.scm
  if [ -e $FILE ]; then
    compile_scheme $FILE
  else
    echo "Error: $FILE does not exist"
  fi
fi

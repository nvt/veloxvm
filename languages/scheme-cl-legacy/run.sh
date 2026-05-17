#!/bin/sh

COMPILER=compiler-cli.lisp

# Check if an environmental variable is set for the Common LISP implementation.
# If not, the default choice is GNU CLISP.
CL_IMPL=${CL_IMPL:-clisp}

case $CL_IMPL in
	"abcl")   CMD_LINE="abcl --load $COMPILER --" ;;
	"ccl")    CMD_LINE="ccl -l $COMPILER --" ;;
	"clisp")  CMD_LINE="clisp -on-error debug $COMPILER" ;;
	"cmucl")  CMD_LINE="lisp -load $COMPILER --" ;;
	"native") CMD_LINE="./compiler" ;;
	"sbcl")   CMD_LINE="sbcl --script $COMPILER" ;;
	*)        echo "Invalid setting of CL_IMPL in run.sh!"
	          exit 1
esac

$CMD_LINE $*

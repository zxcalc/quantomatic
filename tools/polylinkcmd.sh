#!/usr/bin/env bash
#
# Create linking command for making an executable from an PolyML
# object file.
#

ROOT_DIR="$(cd "$(dirname $0)"; cd ..; pwd)";
PRG="$(basename "$0")"

BIN_FILE=$1
OBJ_FILE=$2

if [ -z "$BIN_FILE" ] || [ -z "$OBJ_FILE" ]; then 
    echo "Usage: $PRG BIN_FILE_NAME OBJ_FILE_NAME" >&2
    exit 2; # fail
fi

POLYML_HOME="$($ROOT_DIR/tools/findpoly.sh)"
POLYML_LIB="$POLYML_HOME/lib"

ARCH="$(uname -m)"

if [ "$ARCH" = "i686" ] || [ "$ARCH" = "i586" ] || [ "$ARCH" = "i486" ]; then
    # "$ARCH is a 32 bit architecture."
    SEGPROT=""
elif [ "$ARCH" = "x86_64" ] || [ "$ARCH" = "darwin_64" ]; then
    # "$ARCH is a 64 bit architecture."
    SEGPROT="-segprot POLY rwx rwx"
else
    echo "$PRG: unkown architecture: $ARCH" >&2
    exit 2; # fail
fi

LD_RUN_PATH="$POLYML_LIB:$LD_RUN_PATH"
echo cc $SEGPROT -o $BIN_FILE $OBJ_FILE -L$POLYML_LIB -lpolymain -lpolyml
cc $SEGPROT -o $BIN_FILE $OBJ_FILE -L$POLYML_LIB -lpolymain -lpolyml
#!/usr/bin/env bash
#
# Find PolyML; first tries POLYML_HOME env variable, 
# then searches in local directories (THE_DIR_OF_THIS_SCRIPT/../../polyml)
# then checks the PATH
# then looks at some standard system-installation directories 
#   ("/usr/local/polyml", "/usr/share/polyml", "/opt/polyml")

ROOT_DIR="$(cd "$(dirname $0)"; cd ..; pwd)";
PRG="$(basename "$0")"

# choose from a collection of things
function choosefrom ()
{
	local RESULT=""
	local DIR=""

	for DIR in "$@"
	do
		[ -z "$RESULT" -a -d "$DIR" -a -x "$DIR/bin/poly" ] && RESULT="$DIR"
	done

	[ -z "$RESULT" ] && RESULT="$DIR"
	echo "$RESULT"
}

POLYML_IN_PATH="$(type -p poly)"
if [ -n "$POLYML_IN_PATH" ]; then 
	POLYML_IN_PATH="$(cd $(dirname $POLYML_IN_PATH); cd ..; pwd)"
fi

# polyml from same directory as quantomatic
[ -d "$ROOT_DIR/../polyml" ] && LOCAL_POLY_DIR="$(cd $ROOT_DIR/../polyml; pwd)"
# polyml from contrib directory of quatomatc
[ -d "$ROOT_DIR/tools/contrib/polyml" ] && CONTRIB_POLY_DIR="$(cd $ROOT_DIR/tools/contrib/polyml; pwd)"

POLYML_HOME=$(choosefrom \
	"$POLYML_HOME" \
	"$CONTRIB_POLY_DIR" \
	"$LOCAL_POLY_DIR" \
	"$POLYML_IN_PATH" \
	"/usr/local/polyml" \
	"/usr/share/polyml" \
	"/opt/polyml" \
	"/usr" \
	"")

echo $POLYML_HOME

# vi:ts=4:sts=4:sw=4:noet

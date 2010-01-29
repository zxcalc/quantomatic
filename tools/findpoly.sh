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
  local FILE=""

  for FILE in "$@"
  do
    [ -z "$RESULT" -a -e "$FILE" ] && RESULT="$FILE"
  done

  [ -z "$RESULT" ] && RESULT="$FILE"
  echo "$RESULT"
}

POLYML_HOME=$(choosefrom \
  "$POLYML_HOME" \
  "$ROOT_DIR/../polyml" \
  "$(cd $(dirname $(type -p poly)); cd ..; pwd)" \
  "/usr/local/polyml" \
  "/usr/share/polyml" \
  "/opt/polyml")

echo $POLYML_HOME;
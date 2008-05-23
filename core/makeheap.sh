#!/bin/bash

if [ ! -d $HOME/.quantomatic ]; then echo "Making a new .quantomatic directory!"; mkdir $HOME/.quantomatic; else echo "You already have a .quantomatic dir, hurray!"; fi

if [ -f $HOME/.quantomatic/quanto ]; then echo "Remving old quanto heap file."; rm $HOME/.quantomatic/quanto; fi

echo "Building new quanto heap file in $HOME/.quantomatic/quanto"
isabelle -e "use \"ROOT.ML\"; SaveState.saveState \"$HOME/.quantomatic/quanto\" ; quit ();" HOL_IsaP

echo "Quantomatic heap built!"
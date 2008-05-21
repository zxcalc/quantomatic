#!/bin/bash

rm $HOME/isabelle/heaps/Isabelle2007/polyml-5.1_x86-darwin/quanto
isabelle  -e 'use "ROOT.ML"; SaveState.saveState "quanto" ; quit ();' HOL_IsaP
mv quanto $HOME/isabelle/heaps/Isabelle2007/polyml-5.1_x86-darwin/
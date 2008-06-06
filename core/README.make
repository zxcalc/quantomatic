To make the binary you need to a set a few things up... You need to
have an environment variable POLYLIB so that library linking
succeeds. I suspect this is not needed if you have polyml installed in
a standard place, but for non-standard installs, something like the
following needs to be in your environment setup:

export POLYLIB="/home/ldixon/local/polyml-5.2.0/lib"
export LD_LIBRARY_PATH="$POLYLIB:$LD_LIBRARY_PATH"
export DYLD_LIBRARY_PATH="$POLYLIB:$DYLD_LIBRARY_PATH"
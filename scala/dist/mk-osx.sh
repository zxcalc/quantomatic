#!/bin/bash

# Create OS X application bundle. Note this script should be run
# from the parent directory, i.e. as 'dist/mk-osx.sh'.

BUNDLE=target/QuantoDerive.app/Contents

sbt appbundle
cp -f dist/osx-dist/Info.plist $BUNDLE/
cp -f ../core/run_protocol.ML $BUNDLE/Resources/
cp -f dist/ml.xml $BUNDLE/Resources/

mkdir -p $BUNDLE/Resources/bin
cp -f dist/osx-dist/poly $BUNDLE/Resources/bin/

mkdir -p $BUNDLE/Resources/heaps
cp -f ../core/heaps/quanto.heap $BUNDLE/Resources/heaps/

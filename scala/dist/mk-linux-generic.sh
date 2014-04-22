#!/bin/bash

# Create generic linux/64 bit bundle. Note this script should be run
# from the parent directory, i.e. as 'dist/mk-linux-generic.sh'.

BUNDLE=target/QuantoDerive

sbt package
mkdir -p $BUNDLE/jars
mkdir -p $BUNDLE/bin

cp -f dist/linux-dist/quanto-derive.sh $BUNDLE/
cp -f dist/linux-dist/polybin dist/linux-dist/poly $BUNDLE/bin
cp -f dist/linux-dist/libpolyml.so.4 $BUNDLE/bin

cp -f lib_managed/jars/*/*/*.jar $BUNDLE/jars
cp -f lib_managed/bundles/*/*/*.jar $BUNDLE/jars
cp -f lib/*.jar $BUNDLE/jars
cp -f target/*/quanto*.jar $BUNDLE/jars


# This dummy file lets the frontend know it is running inside
# a generic linux bundle.
touch $BUNDLE/linux-bundle

cp -f ../core/run_protocol.ML $BUNDLE/
mkdir -p $BUNDLE/heaps
cp -f ../core/heaps/quanto.heap $BUNDLE/heaps/

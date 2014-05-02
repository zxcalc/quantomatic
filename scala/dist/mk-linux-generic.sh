#!/bin/bash

# Create generic linux/64 bit bundle. Note this script should be run
# from the parent directory, i.e. as 'dist/mk-linux-generic.sh'.

BUNDLE=target/QuantoDerive

mkdir -p $BUNDLE/jars
mkdir -p $BUNDLE/bin

# This dummy file lets the frontend know it is running inside
# a generic linux bundle.
touch $BUNDLE/linux-bundle

echo Running SBT...
sbt package

echo Including binaries...
cp -f dist/linux-dist/quanto-derive.sh $BUNDLE/
cp -f dist/linux-dist/polybin dist/linux-dist/poly $BUNDLE/bin
cp -f dist/linux-dist/libpolyml.so.4 $BUNDLE/bin

echo Including heap...
cp -f ../core/run_protocol.ML $BUNDLE/
cp -f dist/ml.xml $BUNDLE/
mkdir -p $BUNDLE/heaps
cp -f ../core/heaps/quanto.heap $BUNDLE/heaps/

echo Including jars...
# manually grabbing managed dependencies. maybe easier to let SBT do this?
cp -f lib_managed/jars/*/*/akka-actor*.jar $BUNDLE/jars
cp -f lib_managed/jars/*/*/scala-library*.jar $BUNDLE/jars
cp -f lib_managed/jars/*/*/scala-swing*.jar $BUNDLE/jars
cp -f lib_managed/jars/*/*/jackson-core*.jar $BUNDLE/jars
cp -f lib_managed/bundles/*/*/config*.jar $BUNDLE/jars

# grab local dependences
cp -f lib/*.jar $BUNDLE/jars

# include quanto jar file
cp -f target/*/quanto*.jar $BUNDLE/jars

echo Creating archive...
cd target
tar czf QuantoDerive-linux.tar.gz QuantoDerive
echo Done.

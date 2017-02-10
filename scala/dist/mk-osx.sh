#!/bin/bash

# Create OS X application bundle. Note this script should be run
# from the parent directory, i.e. as 'dist/mk-osx.sh'.

APP=target/QuantoDerive.app
BUNDLE=$APP/Contents

rm -rf $APP

echo Rebuilding the core heap...
(cd ../core; ../scala/dist/osx-dist/poly --use build_heap.ML)

echo Running SBT to make jar...
sbt package

echo Running ant to make app...
ant bundle-quanto

echo Including resources...
# cp -f dist/osx-dist/Info.plist $BUNDLE/
cp -f ../core/run_protocol.ML $BUNDLE/Resources/
cp -f dist/ml.xml $BUNDLE/Resources/
cp -f dist/scala.xml $BUNDLE/Resources/

# This dummy file lets the frontend know it is running inside
# an OS X application bundle.
touch $BUNDLE/Resources/osx-bundle

mkdir -p $BUNDLE/Resources/bin
cp -f dist/osx-dist/poly $BUNDLE/Resources/bin/
cp -f dist/osx-dist/libpolyml.4.dylib $BUNDLE/Resources/bin/

echo Including heap...
mkdir -p $BUNDLE/Resources/heaps
cp -f ../core/heaps/quanto.heap $BUNDLE/Resources/heaps/

echo Signing app...
find $BUNDLE -type f \( -name "*.jar" -or -name "*.dylib" -or -name "poly" \) \
  -exec codesign -v -f -s "-" {} \;
codesign -v -f -s "-" $APP


echo Creating archive...
cd target
zip -r QuantoDerive-osx.zip QuantoDerive.app
echo Done.

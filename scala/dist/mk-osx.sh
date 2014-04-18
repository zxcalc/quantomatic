sbt appbundle
cp -f dist/osx-dist/Info.plist target/QuantoDerive.app/Contents/
cp -f ../core/heaps/quanto.heap target/QuantoDerive.app/Contents/Resources/osx-dist/
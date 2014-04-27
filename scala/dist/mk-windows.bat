@echo off
set BUNDLE=target\QuantoDerive

rmdir /S /Q %BUNDLE%

mkdir %BUNDLE%
mkdir %BUNDLE%\jars
mkdir %BUNDLE%\bin

REM This dummy file lets the frontend know it is running inside
REM a generic linux bundle.
type nul >> %BUNDLE%\windows-bundle

echo Running SBT...
sbt package

REM echo Including binaries...
REM cp -f dist/linux-dist/quanto-derive.sh $BUNDLE/
REM cp -f dist/linux-dist/polybin dist/linux-dist/poly $BUNDLE/bin
REM cp -f dist/linux-dist/libpolyml.so.4 $BUNDLE/bin
REM 
REM echo Including heap...
REM cp -f ../core/run_protocol.ML $BUNDLE/
REM mkdir -p $BUNDLE/heaps
REM cp -f ../core/heaps/quanto.heap $BUNDLE/heaps/
REM 
REM echo Including jars...
REM # manually grabbing managed dependencies. maybe easier to let SBT do this?
REM cp -f lib_managed/jars/*/*/akka-actor*.jar $BUNDLE/jars
REM cp -f lib_managed/jars/*/*/scala-library*.jar $BUNDLE/jars
REM cp -f lib_managed/jars/*/*/scala-swing*.jar $BUNDLE/jars
REM cp -f lib_managed/jars/*/*/jackson-core*.jar $BUNDLE/jars
REM cp -f lib_managed/bundles/*/*/config*.jar $BUNDLE/jars
REM 
REM # grab local dependences
REM cp -f lib/*.jar $BUNDLE/jars
REM 
REM # include quanto jar file
REM cp -f target/*/quanto*.jar $BUNDLE/jars
REM 
REM echo Creating archive...
REM cd target
REM tar czf QuantoDerive-linux.tar.gz QuantoDerive
REM echo Done.

@echo off
set BUNDLE=target\QuantoDerive
set SCALA_VERSION=2.10

rmdir /S /Q %BUNDLE%

mkdir %BUNDLE%
mkdir %BUNDLE%\jars
mkdir %BUNDLE%\bin

REM This dummy file lets the frontend know it is running inside
REM a generic linux bundle.
type nul >> %BUNDLE%\windows-bundle
copy dist\ml.xml %BUNDLE%

echo Rebuilding core heap...
cd ..\core
..\scala\dist\windows-dist\poly.exe --use build_heap.ML
cd ..\scala

echo Running SBT...
call sbt package

echo Including binaries...
copy dist\windows-dist\poly.exe %BUNDLE%\bin
copy dist\windows-dist\libpoly* %BUNDLE%\bin
copy dist\windows-dist\cyg* %BUNDLE%\bin
copy dist\windows-dist\QuantoDerive.exe %BUNDLE%


echo Including heap...
copy ..\core\run_protocol.ML %BUNDLE%
mkdir %BUNDLE%\heaps
copy ..\core\heaps\quanto.heap %BUNDLE%\heaps

echo Including jars...

REM manually grabbing managed dependencies. maybe easier to let SBT do this?
copy lib_managed\jars\com.typesafe.akka\akka-actor_%SCALA_VERSION%\akka-actor*.jar %BUNDLE%\jars
copy lib_managed\jars\org.scala-lang\scala-library\scala-library*.jar %BUNDLE%\jars
copy lib_managed\jars\org.scala-lang\scala-swing\scala-swing*.jar %BUNDLE%\jars
copy lib_managed\jars\com.fasterxml.jackson.core\jackson-core\jackson-core*.jar %BUNDLE%\jars
copy lib_managed\bundles\com.typesafe\config\config*.jar %BUNDLE%\jars

REM grab local dependences
copy lib\*.jar %BUNDLE%\jars

REM include quanto jar file
copy target\scala-%SCALA_VERSION%\quanto*.jar %BUNDLE%\jars


REM echo Creating archive...
REM cd target
REM tar czf QuantoDerive-linux.tar.gz QuantoDerive
echo Done.

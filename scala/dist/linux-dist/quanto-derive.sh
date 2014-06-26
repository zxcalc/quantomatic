#!/bin/bash

DIR="$(dirname "$0")"
CLASSPATH=$(echo jars/*.jar | tr ' ' ':')
echo $CLASSPATH
cd $DIR
java -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled \
 -XX:MaxPermSize=384M -cp $CLASSPATH quanto.gui.QuantoDerive

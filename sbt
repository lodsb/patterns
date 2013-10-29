#!/bin/bash
JAVA_TOOL_OPTIONS='-Dfile.encoding=ISO-8859-1'
java -Dfile.encoding="iso-8859-1" -Xss4M -Xmx1200M -XX:MaxPermSize=512M -XX:NewSize=128M -XX:NewRatio=3 -jar `dirname $0`/sbt-launch.jar "$@"


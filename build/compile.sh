#!/usr/bin/env bash
set -e

SBT_CMD="sbt -DscalaVersion=$SCALA_VERSION ++$SCALA_VERSION"
if [[ $SCALA_VERSION == 2.11* ]]
then
    SBT_CMD+=" coverage test:compile"
elif [[ $SCALA_VERSION == 2.12* ]]
then
    SBT_CMD+=" test:compile"
else
    exit 1
fi

echo Archiving compiled data...
mkdir -p $HOME/compiled
tar -zcvf $HOME/compiled/$SCALA_VERSION.tar.gz $(pwd)

mkdir -p $HOME/compiled/res
tar -zxvf $HOME/compiled/$SCALA_VERSION.tar.gz -C  $HOME/compiled/res

$SBT_CMD
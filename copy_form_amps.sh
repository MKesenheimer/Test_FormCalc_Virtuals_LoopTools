#!/bin/bash

PROCESS=dbard_n1n2g_virt
WORKINGDIR=$(pwd)
SCRIPTS=$WORKINGDIR/Scripts
PROCESSDIR=$SCRIPTS/$PROCESS
DESTINY=$WORKINGDIR/squaredme

rm -f $DESTINY/*.F

cp $PROCESSDIR/squaredme/*.F $DESTINY
cp $PROCESSDIR/squaredme/*.h $DESTINY/include

cp $PROCESSDIR/renconst/*.F $DESTINY
cp $PROCESSDIR/renconst/*.h $DESTINY/include


#!/bin/sh
# calls the LoopTools and SLHAlib configure scripts

TOOLS=$(pwd)/Tools
LT=$WORKINGDIR/LoopTools-2.12

COMPILER=$1
if [ "$1" = "" ]; then
  COMPILER=gfortran
fi

cd $LT && ./configure FC=$COMPILER
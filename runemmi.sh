#!/usr/bin/env bash

if [ ! -d $1 ]
then

  mkdir -p $1

  #/usr/bin/time -o test.out ./em.R --xstart=0 --xend=100000 --ystart=0 --yend=100000 -f $2 --convergence=$3
  /usr/bin/time -o test.out ./em.R --initfraction=0.1 --clusterbracket=1 -f $2 --convergence=$3

  mv test.out *.dat *.png $1

fi


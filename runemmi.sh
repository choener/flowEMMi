#!/usr/bin/env bash

mkdir -p $1

/usr/bin/time -o test.out ./em.R --xstart=0 --xend=100000 --ystart=0 --yend=100000 -f $2 --convergence=$3

mv test.out *.dat $1


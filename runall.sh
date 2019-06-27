#!/usr/bin/env bash

# emmi runs

./runemmi.sh out-flowemmi/012-1e+0 ./fcs/InTH_160713_012.fcs 1e+0
./runemmi.sh out-flowemmi/025-1e+0 ./fcs/InTH_160712_025.fcs 1e+0
./runemmi.sh out-flowemmi/026-1e+0 ./fcs/InTH_160720_026.fcs 1e+0
./runemmi.sh out-flowemmi/039-1e+0 ./fcs/InTH_160719_039.fcs 1e+0

./runemmi.sh out-flowemmi/012-1e-2 ./fcs/InTH_160713_012.fcs 1e-2
./runemmi.sh out-flowemmi/025-1e-2 ./fcs/InTH_160712_025.fcs 1e-2
./runemmi.sh out-flowemmi/026-1e-2 ./fcs/InTH_160720_026.fcs 1e-2
./runemmi.sh out-flowemmi/039-1e-2 ./fcs/InTH_160719_039.fcs 1e-2

./runemmi.sh out-flowemmi/012-1e-5 ./fcs/InTH_160713_012.fcs 1e-5
./runemmi.sh out-flowemmi/025-1e-5 ./fcs/InTH_160712_025.fcs 1e-5
./runemmi.sh out-flowemmi/026-1e-5 ./fcs/InTH_160720_026.fcs 1e-5
./runemmi.sh out-flowemmi/039-1e-5 ./fcs/InTH_160719_039.fcs 1e-5

# merge runs

./runmerge.sh out-flowmerge/012-1e+0 ./fcs/InTH_160713_012.fcs 1e+0
./runmerge.sh out-flowmerge/025-1e+0 ./fcs/InTH_160712_025.fcs 1e+0
./runmerge.sh out-flowmerge/026-1e+0 ./fcs/InTH_160720_026.fcs 1e+0
./runmerge.sh out-flowmerge/039-1e+0 ./fcs/InTH_160719_039.fcs 1e+0

./runmerge.sh out-flowmerge/012-1e-2 ./fcs/InTH_160713_012.fcs 1e-2
./runmerge.sh out-flowmerge/025-1e-2 ./fcs/InTH_160712_025.fcs 1e-2
./runmerge.sh out-flowmerge/026-1e-2 ./fcs/InTH_160720_026.fcs 1e-2
./runmerge.sh out-flowmerge/039-1e-2 ./fcs/InTH_160719_039.fcs 1e-2

./runmerge.sh out-flowmerge/012-1e-5 ./fcs/InTH_160713_012.fcs 1e-5
./runmerge.sh out-flowmerge/025-1e-5 ./fcs/InTH_160712_025.fcs 1e-5
./runmerge.sh out-flowmerge/026-1e-5 ./fcs/InTH_160720_026.fcs 1e-5
./runmerge.sh out-flowmerge/039-1e-5 ./fcs/InTH_160719_039.fcs 1e-5



# out-flowemmi/012-1e-2

#   InTH_160712_002.fcs
#   InTH_160712_010.fcs
#   InTH_160712_011.fcs

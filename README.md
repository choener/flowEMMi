# flowEMMi v0.2

*flowEMMi* is a tool for automated model-based clustering of microbial data. It
is particularly useful in two cases (i) your FCS data contains only few (maybe
two!) channels and (ii) you need a fast tool.

Case (i) will happen quite often with non-human data, when no antibodies are
available and only staining is, yielding foward- and side-scatter and no
additional channels.

Case (ii) happens to everybody working with lots of data, typically those
people dealing with problem (i).

## How run run flowEMMi

Here are a number of ways how to run flowEMMi for your data.

### any linux

We provide a wrapped executable that contains all required dependencies for
simple testing. Please note that this should be used mostly for testing, since
the startup time is quite long. For production use, install the dependencies
and use the *cpp and *.R files directly or use the NixOS version.

TODO nix-bundle

### under NixOS

Should work with nixpkgs under any linux, as well.

- Download the newest package and extract
- cd AutoGating
- nix-shell
  (installation of missing dependencies)
- ./em.R --help
  (to show help)
- ./em.R --log --separation -f <fcs-file.fcs>
  (will compile the *cpp file and produce output files)

## Results

After running the program, there will be:
- png's (optionally svg's) with the cluster visualization
- bic.png with the graph of BIC for each number of clusters: choose the first
  number of cluster, once the graph flattens, for a sparse, good solution
- bic.txt with the textual data
- *txt files with relative fill data for each cluster
- cluster-positions-X.txt with the positions and covariance matrices for each
  cluster

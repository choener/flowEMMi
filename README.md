# flowEMMi v0.2

*flowEMMi* is a tool for automated model-based clustering of microbial data. It is particularly
useful in two cases (i) your FCS data contains only few (maybe two!) channels and (ii) you need a
fast tool.

Case (i) will happen quite often with non-human data, when no antibodies are available and only
staining is, yielding foward- and side-scatter and no additional channels.

Case (ii) happens to everybody working with lots of data, typically those people dealing with
problem (i).

## Usage

A description of the usage of flowEMMi is currently work in progress, since we are actively
converting flowEMMi to a library. The library will then be provided with a small frontend.

## Installation

### R

*flowEMMi* is an R package and can be installed like thus: TODO

### NixOS

Development under NixOS is easy, as this repository is a *Nix flake*. Calling **nix develop** will
provide a development shell with all necessary dependencies installed. This flake is pinned to the a
state thats work on the developers' machines.


# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

with import <nixpkgs> {};
let
  flowEmmiR = rWrapper.override { packages = with rPackages; [ Rcpp RcppEigen optparse flowCore flowViz ggplot2 gtools randomcoloR mvtnorm mixtools ]; };
in
  flowEmmiR


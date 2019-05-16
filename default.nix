# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

with import <nixpkgs> {};
let
  # what we need for our system
  rPemmi = with rPackages; [ Rcpp RcppEigen optparse flowCore flowViz ggplot2 gtools ellipse mixtools mvtnorm colortools tictoc ];

  # packages to compare against
  rFlowMerge = with rPackages;
    let
      biocVersion = "3.8"; # BiocVersion.version;
    in buildRPackage rec {
    name = "flowMerge";
    version = "2.30.1";
    # homepage src = "https://bioconductor.org/packages/${biocVersion}/bioc/html/${name}.html";
    src = fetchurl {
      sha256 = "1dn6wxaix56r3fw273prbajc84h1k1b03q6wh0pfhy0gr6qk8wgw";
      urls = [
        "https://www.bioconductor.org/packages/release/bioc/src/contrib/${name}_${version}.tar.gz"
      ];
    };
    depends = [ graph feature flowClust Rgraphviz foreach snow ];
    propagatedBuildInputs = depends;
    nativeBuildInputs = depends;
  };
  rPcompare = with rPackages; [ flowClust rFlowMerge ];

  # final environment
  flowEmmiR = rWrapper.override { packages = rPemmi ++ rPcompare; };
in
  flowEmmiR


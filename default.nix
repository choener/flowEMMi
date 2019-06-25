# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

 # with import <nixpkgs> {};

{ rWrapper, rPackages, zlib, parallel, libxml2, fetchurl, fetchFromGitHub, recurseIntoAttrs }:

let

  # rPackages to be overriden
  rP = (rPackages.override {
    overrides = (rec {
      ncdfFlow = rPackages.ncdfFlow.overrideAttrs (old: rec {
        depends = old.depends ++ [ zlib ];
      });
      RcppParallel = rPackages.RcppParallel.overrideAttrs (old: rec {
        patchPhase = ''
          patchShebangs configure
        '';
      });
      flowWorkspace = RflowWorkspace;
      #flowWorkspace = rPackages.flowWorkspace.overrideAttrs (old: rec {
      #  nativeBuildInputs = old.nativeBuildInputs ++ [ libxml2 ];
      #});
    }); # overrides
  });

  # what we need for our system
  rPemmi = with rP; [
      Rcpp RcppEigen optparse flowCore flowViz ggplot2 gtools ellipse mixtools mvtnorm colortools tictoc parallel snow Rmpi
      RflowWorkspace
      CytoML
    ];

  # required overrides due to failuers

  RflowWorkspace = with rP;
    let
      biocVersion = "3.8"; # BiocVersion.version;
    in buildRPackage rec {
    name = "flowWorkspace";
    version = "3.30.2";
    # version 3.32.0 does not build
    # below is the "head" version which is way too incompatible
    # src = fetchFromGitHub {
    #   owner = "RGLab";
    #   repo = "flowWorkspace";
    #   rev = "faac59574d3a04c182e8edcf337bc162d81a7ee5";
    #   sha256 = "1xkfh8wjxmm1ygipgghv2rfibvgybbxpcsg4i8q2dlpxdij3d20f";
    # };
    src = fetchurl {
      #sha256 = "1r1xxqzlp2x2gf79f37ckninnyjkxcb7xz7rffg2nadmapl5shvw"; # 3.32.0
      sha256 = "19ifpwpk9rmmfm647zm419k50hna8ib0ad75l04xbggdm6s3vm41";
      urls = [
        "https://www.bioconductor.org/packages/${biocVersion}/bioc/src/contrib/${name}_${version}.tar.gz"
      ];
    };
    depends = [ BH Biobase BiocGenerics cytolib data_table dplyr flowCore flowViz graph gridExtra lattice latticeExtra matrixStats
                ncdfFlow # needs override
                RBGL RColorBrewer Rcpp Rgraphviz RProtoBufLib scales stringr XML
                libxml2 digest RcppParallel # new deps
              ];
    propagatedBuildInputs = depends;
    nativeBuildInputs = depends;
  };

  # packages to compare against
  rFlowMerge = with rP;
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
  rPcompare = with rP; [ flowClust rFlowMerge ];

  # final environment
  flowEmmiR = rWrapper.override { packages = rPemmi ++ rPcompare; };
in
  flowEmmiR


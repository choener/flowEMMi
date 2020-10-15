# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

 # with import <nixpkgs> {};

{ rWrapper, rstudioWrapper, rPackages, zlib, parallel, libxml2, fetchurl, fetchFromGitHub, recurseIntoAttrs, qt5, stdenv, lib, R }:

let

  # rPackages to be overriden
  rP = (rPackages.override {
    overrides = (rec {
      flowCore=rPackages.buildRPackage rec {
        name="flowCore";
        version="1.50.0";
        src=fetchurl {
          sha256="0pvcyzycsmgc8iw60q9xnhllfan6ihwpz3gvk8h1n9jmhpxzylan";
          urls=["https://bioconductor.org/packages/3.9/bioc/src/contrib/flowCore_1.50.0.tar.gz"];
        };
        propagatedBuildInputs=with rP; [Biobase BiocGenerics graph rrcov corpcor Rcpp matrixStats MASS BH];
      };
      BiocGenerics=rPackages.buildRPackage rec {
        name="BiocGenerics";
        version="0.30.0";
        src=fetchurl {
          sha256="1n87686bg5nmpqdpzwv1h551dkbxp9wk6wbmzpkgm71qxnk2yv9f";
          urls=["https://bioconductor.org/packages/3.9/bioc/src/contrib/BiocGenerics_${version}.tar.gz"];
        };
        depends=[];
      };
      ncdfFlow = rPackages.ncdfFlow.overrideAttrs (old: rec {
        nativeBuildInputs = old.nativeBuildInputs ++ [zlib];
      });
      RcppParallel = rPackages.RcppParallel.overrideAttrs (old: rec {
        patchPhase = ''
          patchShebangs configure
        '';
      });
      #flowWorkspace = RflowWorkspace;
      #flowWorkspace = rPackages.flowWorkspace.overrideAttrs (old: rec {
      #  nativeBuildInputs = old.nativeBuildInputs ++ [ libxml2 ];
      #});
    }); # overrides
  }); # override r packages

  # nixGL system to run opengl anywhere (testing)
  nixGL =
    let src = fetchFromGitHub {
          owner  = "guibou";
          repo   = "nixGL";
          rev    = "fad15ba09de65fc58052df84b9f68fbc088e5e7c";
          sha256 = "1wc5gfj5ymgm4gxx5pz4lkqp5vxqdk2njlbnrc1kmailgzj6f75h";
        };
    in ((import "${src}/default.nix") {
    });

  # what we need for our system
  rPemmi = with rP; [
      flowCore
      flowViz
      colortools
      devtools
      roxygen2
      Rcpp
      RcppEigen
      optparse
      ggplot2
      gtools
      #ellipse
      mixtools
      #mvtnorm
      tictoc
      #parallel
      #snow
      #Rmpi
      #RflowWorkspace
      #CytoML
      flowCyBar
      flowCHIC
      rgl
    ];

  # required overrides due to failuers

  RflowWorkspace = with rP;
    let
      biocVersion = "3.8"; # BiocVersion.version;
    in buildRPackage rec {
    name = "flowWorkspace";
    version = "3.30.2";
    src = fetchurl {
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
  #rFlowMerge = with rP;
  #  let
  #    biocVersion = "3.8"; # BiocVersion.version;
  #  in buildRPackage rec {
  #  name = "flowMerge";
  #  version = "2.30.1";
  #  # homepage src = "https://bioconductor.org/packages/${biocVersion}/bioc/html/${name}.html";
  #  src = fetchurl {
  #    sha256 = "1dn6wxaix56r3fw273prbajc84h1k1b03q6wh0pfhy0gr6qk8wgw";
  #    urls = [
  #      "https://www.bioconductor.org/packages/release/bioc/src/contrib/${name}_${version}.tar.gz"
  #    ];
  #  };
  #  depends = [ graph feature flowClust Rgraphviz foreach snow ];
  #  propagatedBuildInputs = depends;
  #  nativeBuildInputs = depends;
  #};
  rPcompare = with rP; []; # flowClust rFlowMerge SamSPECTRAL flowMeans ];

  # final environment
  flowEmmiR = rWrapper.override { packages = rPemmi ++ rPcompare; };
  flowEmmiStudio = rstudioWrapper.override { packages = rPemmi ++ rPcompare; };
in
  { flowEmmiR = stdenv.mkDerivation {
      name = "cli";
      buildInputs = with nixGL; [ flowEmmiR nixGLIntel nixGLNvidia ];
    };
    flowEmmiStudio = stdenv.mkDerivation {
      name = "studio";
      buildInputs = with nixGL; [ R rPemmi rPcompare flowEmmiStudio qt5.qtbase nixGLIntel nixGLNvidia ];
      QT_XCB_GL_INTEGRATION="none";
    };
  }


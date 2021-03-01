# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{ R, rWrapper, rstudioWrapper, rPackages
, qt5, zlib, libxml2, hdf5
, parallel
, fetchurl, fetchFromGitHub, recurseIntoAttrs
, pkgs, lib, stdenv, fontconfig
}:

let

  biocversion = "3.9";

  # rPackages to be overriden
  rP = (rPackages.override {
    overrides = (rec {
      flowCore = rPackages.buildRPackage rec {
        name = "flowCore";
        version = "1.50.0";
        src = fetchurl {
          sha256 = "0pvcyzycsmgc8iw60q9xnhllfan6ihwpz3gvk8h1n9jmhpxzylan";
          urls = [ "https://bioconductor.org/packages/${biocversion}/bioc/src/contrib/${name}_${version}.tar.gz" ];
        };
        propagatedBuildInputs = with rP; [ Biobase BiocGenerics graph rrcov corpcor Rcpp matrixStats MASS BH ];
      };
      BiocGenerics = rPackages.buildRPackage rec {
        name = "BiocGenerics";
        version = "0.30.0";
        src = fetchurl {
          sha256 = "1n87686bg5nmpqdpzwv1h551dkbxp9wk6wbmzpkgm71qxnk2yv9f";
          urls = ["https://bioconductor.org/packages/${biocversion}/bioc/src/contrib/${name}_${version}.tar.gz"];
        };
        depends = [];
      };
      ncdfFlow = rPackages.ncdfFlow.overrideAttrs (old: rec {
        propagatedBuildInputs = with rP; [ flowCore RcppArmadillo flowViz zlibbioc Rhdf5lib ];
        nativeBuildInputs = [ hdf5 ];
        depends = [];
      });
      Rhdf5lib = rPackages.buildRPackage rec {
        name = "Rhdf5lib";
        version = "1.6.3";
        src = fetchurl {
          sha256 = "/neHC2v5I/fHnS0x/mns2S892+pzI7miuinxU2WxyGA=";
          urls = ["https://bioconductor.org/packages/${biocversion}/bioc/src/contrib/${name}_${version}.tar.gz"];
        };
        nativeBuildInputs = [ zlib zlib.dev hdf5 hdf5.dev ];
      };
      flowWorkspace = rPackages.flowWorkspace.overrideAttrs (old: rec {
        propagatedBuildInputs = with rP; [ flowCore ncdfFlow RBGL XML gridExtra Rgraphviz data_table dplyr stringr scales RProtoBufLib cytolib ];
        nativeBuildInputs = [ libxml2 ];
        depends = [];
      });
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
    in ((import "${src}/default.nix") { inherit pkgs; });

  # what we need for our system
  rPemmi = with rP; [
      colortools
      devtools
      flowCHIC
      flowCore
      flowCyBar
      flowViz
      flowWorkspace
      ggplot2
      gplots
      gtools
      here
      magick
      mixtools
      optparse
      Rcpp
      RcppEigen
      rgl
      roxygen2
      tictoc
      viridis
    ];

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
  flowEmmiStudio = rstudioWrapper.override { packages = rPemmi ++ rPcompare ++ [ rPackages.extrafont ] ; };
  fontconfig-file = pkgs.writeText "fonts.cfg" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig>
    <cachedir prefix="xdg">fontconfig</cachedir>
    <dir>${pkgs.ankacoder}/share/fonts/</dir>
    <dir>${pkgs.bakoma_ttf}/share/fonts/</dir>
    <dir>${pkgs.cm_unicode}/share/fonts/</dir>
    <dir>${pkgs.liberation_ttf}/share/fonts/</dir>
    <dir>${pkgs.aileron}/share/fonts/</dir>
    </fontconfig>
  '';
in
  { flowEmmiR = stdenv.mkDerivation {
      name = "cli";
      buildInputs = with nixGL; [ flowEmmiR nixGLIntel ];
    };
    flowEmmiStudio = pkgs.mkShell {
      buildInputs = with nixGL; [ R rPemmi rPcompare flowEmmiStudio qt5.qtbase nixGLIntel ];
        QT_XCB_GL_INTEGRATION="none";
        FONTCONFIG_FILE = fontconfig-file; # "${fontconfig.out}/etc/fonts/fonts.conf";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      shellHook = ''
      '';
    };
  }


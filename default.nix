# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{ R, rWrapper, rstudioWrapper, rPackages
, qt5, zlib, libxml2, hdf5
, parallel
, fetchurl, fetchFromGitHub, recurseIntoAttrs
, pkgs, lib, stdenv, fontconfig
}:

let

  biocversion = "3.11";

  # rPackages to be overriden
  rP = (rPackages.override {
    overrides = (rec {
      ncdfFlow = rPackages.ncdfFlow.overrideAttrs (old: rec {
        propagatedBuildInputs = with rP; [ flowCore RcppArmadillo flowViz zlibbioc Rhdf5lib ];
        nativeBuildInputs = [ hdf5 ];
        depends = [];
      });
      Rhdf5lib = rPackages.buildRPackage rec {
        name = "Rhdf5lib";
        version = "1.10.1";
        src = fetchurl {
          sha256 = "yTJKFRfU6EdaH53OX3MGG1VozEDXUzVZJd3IvzPWhTg=";
          urls = ["https://bioconductor.org/packages/${biocversion}/bioc/src/contrib/${name}_${version}.tar.gz"];
        };
        nativeBuildInputs = [ zlib zlib.dev hdf5 hdf5.dev ];
      };
      cytolib = rPackages.buildRPackage rec {
        name = "cytolib";
        version = "2.0.3";
        src = fetchurl {
          sha256 = "kuo3rrtMPE3D3Z6pE4Ym2/DXAgRoaDVQQxnh6ikPbYg=";
          urls = ["https://bioconductor.org/packages/${biocversion}/bioc/src/contrib/${name}_${version}.tar.gz"];
        };
        propagatedBuildInputs = with rP; [ RcppParallel RProtoBufLib Rcpp BH Rhdf5lib RcppArmadillo ];
        buildInputs = [ pkgs.autoreconfHook pkgs.R hdf5 ];
      };
      flowWorkspace = rPackages.flowWorkspace.overrideAttrs (old: rec {
        propagatedBuildInputs = with rP; [ flowCore ncdfFlow RBGL XML gridExtra Rgraphviz data_table dplyr stringr scales RProtoBufLib cytolib ggplot2 ];
        nativeBuildInputs = [ libxml2 hdf5 ];
        depends = [];
      });
      CytoML = rPackages.CytoML.overrideAttrs (old: rec {
        propagatedBuildInputs = with rP;
          [ cytolib flowCore flowWorkspace openCyto XML data_table jsonlite
            RBGL Rgraphviz Biobase graph base64enc plyr dplyr ggcyto yaml
            lattice corpcor RUnit tibble RcppParallel xml2 Rcpp BH RProtoBufLib RcppArmadillo
          ];
        nativeBuildInputs = [ hdf5 libxml2 ];
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
      CytoML
      devtools
      flowCHIC
      flowCore
      flowCyBar
      flowUtils
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


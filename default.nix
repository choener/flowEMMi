# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{ R, rstudio, rPackages
, qt5, zlib, libxml2, hdf5
, parallel
, fetchurl, fetchFromGitHub, recurseIntoAttrs, runCommand, callPackage, lndir, makeWrapper
, pkgs, lib, stdenv, fontconfig
, rWrapper, rstudioWrapper
}:

let

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
  rPemmi = with rPackages; [
      Cairo
      colortools
      CytoML
      devtools
      dynr
      extrafont
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



  # final environment
  flowEmmiR = rWrapper.override { packages = rPemmi; };
  flowEmmiStudio = rstudioWrapper.override { packages = rPemmi; };
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
    <dir>${pkgs.corefonts}/share/fonts/</dir>
    </fontconfig>
  '';
in
  { flowEmmiR = stdenv.mkDerivation {
      name = "cli";
      buildInputs = with nixGL; [ flowEmmiR nixGLIntel ];
    };
    flowEmmiStudio = pkgs.mkShell {
      buildInputs = with nixGL; [ rPemmi flowEmmiStudio qt5.qtbase nixGLIntel ];
        QT_XCB_GL_INTEGRATION="none";
        FONTCONFIG_FILE = fontconfig-file; # "${fontconfig.out}/etc/fonts/fonts.conf";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        #R_LIBS_SITE="${flowEmmiStudio}/library";
      shellHook = ''
      '';
    };
  }


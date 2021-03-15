# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{ R, rstudio, rPackages
, qt5, zlib, libxml2, hdf5
, parallel
, fetchurl, fetchFromGitHub, recurseIntoAttrs, runCommand, callPackage, lndir, makeWrapper
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
      CytoML = rPackages.buildRPackage rec {
        name = "CytoML";
        version = "2.0.5";
        src = fetchurl {
          sha256 = "tpSfZVGcKqcvJFRF4zbxEkH+FbTQPNc8BanWI8DOi5w=";
          urls = ["https://bioconductor.org/packages/${biocversion}/bioc/src/contrib/${name}_${version}.tar.gz"];
        };
        propagatedBuildInputs = with rP;
          [ cytolib flowCore flowWorkspace openCyto XML data_table jsonlite
            RBGL Rgraphviz Biobase graph base64enc plyr dplyr ggcyto yaml
            lattice corpcor RUnit tibble RcppParallel xml2 Rcpp BH RProtoBufLib RcppArmadillo
          ];
        nativeBuildInputs = [ hdf5 libxml2 ];
      };
      #CytoML = rPackages.CytoML.overrideAttrs (old: rec {
      #  propagatedBuildInputs = with rP;
      #    [ cytolib flowCore flowWorkspace openCyto XML data_table jsonlite
      #      RBGL Rgraphviz Biobase graph base64enc plyr dplyr ggcyto yaml
      #      lattice corpcor RUnit tibble RcppParallel xml2 Rcpp BH RProtoBufLib RcppArmadillo
      #    ];
      #  nativeBuildInputs = [ hdf5 libxml2 ];
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

  # <https://discourse.mc-stan.org/t/rstan-on-nixos/17048/12>
  rWrapperNew = { runCommand, R, makeWrapper, lndir, recommendedPackages, packages }:
    runCommand (R.name + "-wrapper") {
      preferLocalBuild = true;
      allowSubstitutes = false;
      buildInputs = [R] ++ recommendedPackages ++ packages;
      nativeBuildInputs = [makeWrapper];
      # Make the list of recommended R packages accessible to other packages such as rpy2
      # (Same as in the original rWrapper)
      passthru = { inherit recommendedPackages; };
    }
    # Wrap a site lib, similar to symlinkJoin but without propagating buildInputs.
    ''
      mkdir -p $out/library
      for lib in $(echo -n $R_LIBS_SITE | sed -e 's/:/\n/g'); do
        ${lndir}/bin/lndir -silent $lib $out/library/
      done
      mkdir -p $out/bin
      cd ${R}/bin
      for exe in *; do
        makeWrapper ${R}/bin/$exe $out/bin/$exe \
          --prefix "R_LIBS_SITE" ":" "$out/library"
      done
    '';

  rWrapper = lib.makeOverridable rWrapperNew {
    inherit runCommand R makeWrapper lndir;
    recommendedPackages = with rP;
      [ boot class cluster codetools foreign KernSmooth lattice MASS
        Matrix mgcv nlme nnet rpart spatial survival
      ];
    packages = [];
  };

  rStudioWrapperNew = { lib, runCommand, rstudio, wrapQtAppsHook, recommendedPackages, packages, qtbase }:
    let
      qtVersion = with lib.versions; "${major qtbase.version}.${minor qtbase.version}";
      Rwrapped = rWrapper.override { packages = recommendedPackages ++ packages; };
    in
    runCommand (rstudio.name + "-wrapper") {
      preferLocalBuild = true;
      allowSubstitutes = false;

      nativeBuildInputs = [wrapQtAppsHook];
      dontWrapQtApps = true;

      buildInputs = [Rwrapped rstudio]; # ++ recommendedPackages ++ packages;

      # rWrapper points R to a specific set of packages by using a wrapper
      # (as in https://nixos.org/nixpkgs/manual/#r-packages) which sets
      # R_LIBS_SITE.  Ordinarily, it would be possible to make RStudio use
      # this same set of packages by simply overriding its version of R
      # with the wrapped one, however, RStudio internally overrides
      # R_LIBS_SITE.  The below works around this by turning R_LIBS_SITE
      # into an R file (fixLibsR) which achieves the same effect, then
      # uses R_PROFILE_USER to load this code at startup in RStudio.
      fixLibsR = "fix_libs.R";
    }
    ''
    R_LIBS_SITE=${Rwrapped}/library}
    mkdir $out
    ln -s ${rstudio}/share $out
    echo "# Autogenerated by wrapper-rstudio.nix from R_LIBS_SITE" > $out/$fixLibsR
    echo -n ".libPaths(c(.libPaths(), \"" >> $out/$fixLibsR
    echo -n $R_LIBS_SITE | sed -e 's/:/", "/g' >> $out/$fixLibsR
    echo -n "\"))" >> $out/$fixLibsR
    echo >> $out/$fixLibsR
    makeQtWrapper ${rstudio}/bin/rstudio $out/bin/rstudio \
      --set R_PROFILE_USER $out/$fixLibsR
    cp ${Rwrapped}/bin/R $out/bin/
    ln -s ${Rwrapped}/library $out/library
    '';

  rstudioWrapper = lib.makeOverridable rStudioWrapperNew {
    inherit runCommand lib rstudio;
    wrapQtAppsHook = qt5.wrapQtAppsHook;
    qtbase = qt5.qtbase;
    recommendedPackages = with rP;
      [ boot class cluster codetools foreign KernSmooth lattice MASS
        Matrix mgcv nlme nnet rpart spatial survival
      ];
    packages = [];
  };

  # final environment
  flowEmmiR = rWrapper.override { packages = rPemmi; };
  flowEmmiStudio = rstudioWrapper.override { packages = rPemmi ++ [ rPackages.extrafont ] ; };
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
      buildInputs = with nixGL; [ rPemmi flowEmmiStudio qt5.qtbase nixGLIntel ];
        QT_XCB_GL_INTEGRATION="none";
        FONTCONFIG_FILE = fontconfig-file; # "${fontconfig.out}/etc/fonts/fonts.conf";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        R_LIBS_SITE="${flowEmmiStudio}/library";
      shellHook = ''
      '';
    };
  }


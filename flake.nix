{
  description = ''
    flowEmmi flake-based development environment.
    This flake takes the place of the "fixed-...nix" derivations. Here, the flake system makes
    sure that dependencies are pinned.
  '';

  inputs = {
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    flake-utils.url = "github:numtide/flake-utils";
    rPackages.url = "github:choener/flake-rPackages";
    rPackages.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, rPackages }:
    # provides "R" for all known system environments
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs {
              inherit system;
              overlays = [ rPackages.overlay rPackages.overlays.flowCytometry self.overlay ];
              config.allowUnfree = true;
            };
            packages = with pkgs.rPackages;
              [ CytoML devtools RcppEigen tictoc
                extrafont showtext Cairo # these are for playing with fonts, not really necessary
                # TODO dynr needs gsl; until I update flake-rPackages, let's do this ...
                (dynr.overrideAttrs (old: { nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.gsl]; }))
                mltools
              ];
            rstudio = pkgs.rstudioWrapper.override {inherit packages;};
            r = pkgs.rWrapper.override {inherit packages;};
            rstudiowrapper = pkgs.writeScriptBin "studio" ''
              RSTUDIO_CHROMIUM_ARGUMENTS="--disable-gpu" ${rstudio}/bin/rstudio $@
            '';
            test = import ./studio.nix { nixpkgs = pkgs; };
            fontconfig-file = pkgs.writeText "fonts.cfg" ''
              <?xml version="1.0" encoding="UTF-8"?>
              <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
              <fontconfig>
              <cachedir prefix="xdg">fontconfig</cachedir>
              <dir>${pkgs.corefonts}/share/fonts/</dir>
              <dir>${pkgs.liberation_ttf}/share/fonts/</dir>
              <dir>${pkgs.carlito}/share/fonts/</dir>
              <dir>${pkgs.vistafonts}/share/fonts/</dir>
              </fontconfig>
            '';
        in {
          devShell = pkgs.mkShell {
            name = "testEnv";
            packages = [ (pkgs.rWrapper.override {inherit packages;}) ];
            FONTCONFIG_FILE=fontconfig-file;
          }; # devShell
          apps = {
            rstudio = { type="app"; program="${rstudio}/bin/rstudio"; };
            #test = { type="app"; program="${test}"; };
          };
          packages = { inherit test; };
        }
      ) //
    # provide Rstudio only for x86_64
    {
#      studio = let pkgs = nixpkgs.legacyPackages.x86_64-linux;
#               in  import ./studio.nix { nixpkgs = pkgs; };
    } //
    {
      overlay = final: prev: {};
    };
}

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
        # the order of overlays is important!
        let pkgs = import nixpkgs { inherit system; overlays = [ rPackages.overlay rPackages.overlays.flowCytometry ]; };
        in {
          devShell = pkgs.stdenv.mkDerivation {
            name = "testEnv";
            nativeBuildInputs = let packages = with pkgs.rPackages; [CytoML devtools RcppEigen tictoc Phenoflow];
            in [
              (pkgs.rstudioWrapper.override {inherit packages;})
              (pkgs.rWrapper.override {inherit packages;})
            ];
          }; # devShell
        }
      ) //
    # provide Rstudio only for x86_64
    {
      studio = let pkgs = nixpkgs.legacyPackages.x86_64-linux;
               in  import ./studio.nix { nixpkgs = pkgs; };
    };
}

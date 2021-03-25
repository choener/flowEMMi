{
  description = ''
    flowEmmi flake-based development environment.
    This flake takes the place of the "fixed-...nix" derivations. Here, the flake system makes
    sure that dependencies are pinned.
  '';

  inputs = {
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    flake-utils.url = "github:numtide/flake-utils";
    rPackages.url = "github:choener/flake-rPackages";
    rPackages.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, rPackages }:
    # provides "R" for all known system environments
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs { inherit system; overlays = [ self.overlay rPackages.overlay ]; };
        in {
          devShell = pkgs.stdenv.mkDerivation {
            name = "testEnv";
            nativeBuildInputs = [
              (pkgs.rstudioWrapper.override {packages = [pkgs.rPackages.CytoML];})
              (pkgs.rWrapper.override {packages = [pkgs.rPackages.CytoML];})
            ];
          }; # devShell
        }
      ) //
    # provide Rstudio only for x86_64
    {
      studio = let pkgs = nixpkgs.legacyPackages.x86_64-linux;
               in  import ./studio.nix { nixpkgs = pkgs; };
    } //
    {
      overlay = final: prev: {};
    };
}

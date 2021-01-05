{
  description = ''
    flowEmmi flake-based development environment.
    This flake takes the place of the "fixed-...nix" derivations. Here, the flake system makes
    sure that dependencies are pinned.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # provides "R" for all known system environments
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShell = import ./shell.nix { nixpkgs = pkgs; };
        }
      ) //
    # provide Rstudio only for x86_64
    {
      studio = let pkgs = nixpkgs.legacyPackages.x86_64-linux;
               in  import ./studio.nix { nixpkgs = pkgs; };
    };
}

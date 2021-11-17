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
        let pkgs = import nixpkgs { inherit system; overlays = [ rPackages.overlay self.overlay ]; };
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
    } //
    {
      overlay = final: prev: {
        # testing overlay for Phenoflow package
        rPackages = prev.rPackages.override { overrides = {
          flowFDA = final.rPackages.buildRPackage {
            name = "flowFDA";
            src = final.fetchFromGitHub {
              owner = "statOmics";
              repo = "flowFDA";
              #rev = "03aba8876eaf401f0cdc0f22f00e4005f7550534";
              #sha256 = "sha256-f+CNPZIl9ngIPoH5ShdCTHlbNf8D2aXUAWd4vyStZm0=";
              # this is only a pull request!
              # https://github.com/statOmics/flowFDA/pull/1/commits
              rev = "b6d8ec6e4ab760988821d99dcced32c6635d1d49";
              sha256 = "sha256-tAJyEt/DtvOzrxr6URsOemYtL9kQsLz7f2Xseeh1sU0=";
            };
            propagatedBuildInputs = with final.pkgs.rPackages; [ Biobase BiocGenerics flowViz flowFP MASS multcomp mclust devtools ];
          };
          Phenoflow = final.rPackages.buildRPackage {
            name = "Phenoflow";
            src = final.fetchFromGitHub {
              owner = "CMET-UGent";
              repo = "Phenoflow_package";
              rev = "e8d096bc5f86718cafb5914017a067fedbfc707e";
              sha256 = "sha256-M1TZ+v+CDb9O6rerGxilYptVfEnKCLvw/PMzzTL4mw8=";
            };
            propagatedBuildInputs = with final.pkgs.rPackages; [ flowCore flowClean flowFDA flowAI foreach vegan MESS boot cowplot gridExtra e1071 data_table phyloseq caret randomForest doParallel tidyr ];
          }; # Phenoflow
          rhdf5filters = prev.rPackages.rhdf5filters.overrideAttrs (old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [ prev.zlib ];
          });
        };};
      };
    };
}

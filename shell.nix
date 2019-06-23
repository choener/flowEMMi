# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{ nixpkgs ? import <nixpkgs> {} }:
#{ nixpkgs ? import <unstable> {} }:

# working nixos 19.03: 30a82bba734

# TODO fix the import
#nixpkgs = import (builtins.fetchTarball {
#  # Descriptive name to make the store path easier to identify
#  name = "nixpkgs-flowEMMi";
#  # Commit hash for nixos-unstable as of 2018-09-12
#  url = https://github.com/nixos/nixpkgs/archive/ca2ba44cab47767c8127d1c8633e2b581644eb8f.tar.gz;
#  # Hash obtained using `nix-prefetch-url --unpack <url>`
#  sha256 = "1jg7g6cfpw8qvma0y19kwyp549k1qyf11a5sg6hvn6awvmkny47v";
#}) {};

with nixpkgs;

let
  flowEmmiR = callPackage ./. {};
in
  flowEmmiR


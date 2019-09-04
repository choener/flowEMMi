# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{ nixpkgs ? import <nixpkgs> {} }:

# CHzS: these are some nix url suffixes that can be used for pinning. They come
# without the sha, since we trust github not to modify the repository history.

# working nixos 19.03: 30a82bba734
# 2019-09-04: d6bdaea5dd2fb1c9a7c363b0fda17f3f432e7fa6

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
  pkg = callPackage ./. {};
in
  pkg.flowEmmiStudio


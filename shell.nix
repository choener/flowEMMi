# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{ nixpkgs ? import <nixpkgs> {} }:

let
  flowEmmiR = import ./.;
in
  flowEmmiR


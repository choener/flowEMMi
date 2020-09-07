# just calling "nix-shell" yields a shell where everything is available, but we
# need to call ./R explicitly

{}:

# CHzS: these are some nix url suffixes that can be used for pinning. They come
# without the sha, since we trust github not to modify the repository history.

# working nixos 19.03: 30a82bba734
# 2019-09-04: d6bdaea5dd2fb1c9a7c363b0fda17f3f432e7fa6


let
  fixed = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixpkgs-flowEMMi";
    # Commit hash
    url = https://github.com/nixos/nixpkgs/archive/f0924dbf552e28ee0462b180116135c187eb41b4.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0a2bgisfdj73p6911d0mlgx65z7djc8lgwvxqpaskn82in8ka3wg";
  }) {};

  pkg = fixed.callPackage ./. {};
in
  pkg.flowEmmiR


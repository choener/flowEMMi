#!/usr/bin/env nix-shell
#!nix-shell -I /nix/var/nix/profiles/per-user/choener/channels/1903 shell.nix -i bash

echo $@

cd /home/choener/Documents/University/active/flowEMMi
./em.R $@

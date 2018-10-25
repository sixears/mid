#!/bin/bash

echo cabal2nix . '>' cabal.nix
cabal2nix . > cabal.nix
hlint src bin t
for i in $( find proto -type f -name \*.hs -printf '%P\n' ); do
  echo nix-shell --command '${env_replace}' env-replace.nix env-replace.nix '<' proto/$i '>' src/$i
  nix-shell --command '${env_replace}' env-replace.nix env-replace.nix < proto/$i > src/$i
done

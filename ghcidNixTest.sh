#!/usr/bin/env bash
if which nix; then
    nix-shell --run "ghcid --command='cabal v2-repl blog3000-app-test' --test=Test.main"
    exit 0
elif which stack; then
    ghcid '--command=stack ghci' --test='Main.main'
    exit 0
fi

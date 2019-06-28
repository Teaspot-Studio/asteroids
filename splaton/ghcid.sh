nix-shell -A shells.ghc ../default.nix --command "ghcid -c 'cabal new-repl splaton --ghc-options -Wall --ghc-options -Werror'"

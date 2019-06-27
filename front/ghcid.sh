nix-shell -A shells.ghc ../default.nix --command "ghcid -c 'cabal new-repl --ghc lib:asteroids-front'"

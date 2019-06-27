{ minimize ? false }:
let
  reflex-platform = import ./reflex-platform.nix {};
  pkgs = reflex-platform.nixpkgs;
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    asteroids = ./game;
    asteroids-back = ./back;
    asteroids-front = ./front;
  };
  shells = {
    ghcjs = ["asteroids" "asteroids-front"];
    ghc = ["asteroids" "asteroids-front" "asteroids-back"]; # We can build front by GHC to enable use of IDE tools for it
  };
  overrides = import ./overrides.nix { inherit reflex-platform minimize; };
})

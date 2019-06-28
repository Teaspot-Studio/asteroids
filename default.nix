{ minimize ? false }:
let
  reflex-platform = import ./reflex-platform.nix {};
  pkgs = reflex-platform.nixpkgs;
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    asteroids = ./game;
    asteroids-back = ./back;
    asteroids-front = ./front;
    pixi = ./pixi;
    splaton = ./splaton;
  };
  shells = {
    ghcjs = ["asteroids" "asteroids-front" "pixi" "splaton"];
    ghc = ["asteroids" "asteroids-front" "asteroids-back" "pixi" "splaton"]; # We can build front by GHC to enable use of IDE tools for it
  };
  overrides = import ./overrides.nix { inherit reflex-platform minimize; };
})

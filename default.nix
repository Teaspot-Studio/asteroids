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
    matter = ./matter;
  };
  shells = {
    ghcjs = ["asteroids" "asteroids-front" "pixi" "splaton" "matter"];
    ghc = ["asteroids" "asteroids-front" "asteroids-back" "pixi" "splaton" "matter"]; # We can build front by GHC to enable use of IDE tools for it
  };
  overrides = import ./overrides.nix { inherit reflex-platform minimize; };
  shellToolOverrides = ghc: super: {
    inherit (pkgs) closurecompiler;
  };
})

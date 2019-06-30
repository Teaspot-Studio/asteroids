# Here you can put overrides of dependencies
{ reflex-platform ? (import ./reflex-platform.nix {}), minimize ? false, ... }:
let
  pkgs = reflex-platform.nixpkgs;
  lib = pkgs.haskell.lib;
  overrideCabal = lib.overrideCabal;
  dontCheck = lib.dontCheck;
  optimizeGhcjs = drv: overrideCabal drv (drv: { buildFlags = (drv.buildFlags or []) ++ ["--ghcjs-option=-O2 " "--ghcjs-option=-dedupe"]; });
  statics = ./front/statics;
  runClosureCompiler = drv: lib.overrideCabal drv (drv: {
    postFixup = ''
      cd $out/bin/asteroids-front.jsexe
      ${pkgs.closurecompiler}/bin/closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS \
        --externs=all.js.externs \
        --externs=${statics}/js/pixi.min.js \
        --externs=${statics}/js/matter.min.js \
        --externs=${statics}/js/matter-wrap.min.js \
        --jscomp_off=duplicate \
        --jscomp_off=undefinedVars \
        --jscomp_off=externsValidation \
        > all.min.js
        ${pkgs.zopfli}/bin/zopfli -i1000 all.min.js
        '';
  });
in (self: super: let
  prodOverride = drv: if minimize then runClosureCompiler (optimizeGhcjs drv) else drv;
  in {
    asteroids-front = prodOverride super.asteroids-front;
    bytes = dontCheck super.bytes;
    linear = dontCheck super.linear;
    base-compat-batteries = dontCheck super.base-compat-batteries;
    Glob = dontCheck super.Glob;
    apecs = dontCheck super.apecs;
  }
)

# Development build of GHCJS version of frontend
cabal new-build exe:asteroids-front --ghcjs-option=-O2 --ghcjs-option=-dedupe
build_path=dist-newstyle/build/x86_64-linux/ghcjs-8.4.0.1/asteroids-front-1.0.0.0/x/asteroids-front/build/asteroids-front/asteroids-front.jsexe
cp $build_path/all.js ./statics/js/all.js
closure-compiler $build_path/all.js --compilation_level=ADVANCED_OPTIMIZATIONS \
  --externs=$build_path/all.js.externs \
  --externs=./statics/js/pixi.min.js \
  --externs=./statics/js/matter.min.js \
  --jscomp_off=duplicate \
  --jscomp_off=undefinedVars \
  --jscomp_off=externsValidation \
  > ./statics/js/all.min.js

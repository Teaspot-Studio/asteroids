# Development build of GHCJS version of frontend
cabal new-build exe:asteroids-front
cp dist-newstyle/build/x86_64-linux/ghcjs-8.4.0.1/asteroids-front-1.0.0.0/x/asteroids-front/build/asteroids-front/asteroids-front.jsexe/all.js ./statics/js/all.js

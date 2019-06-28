# Asteroids

Simple client-server game that uses GHCJS as client side.

# Hacking

To start hacking:
0. Install nix from https://nixos.org/nix/
1. Run to enter backend shell `./back.sh`
2. Start server `./serve.sh`
3. In another terminal session run `./front.sh`
4. Call `./build.sh` to build frontend part.
5. Open http://localhost:8080/index.html

There is also several `ghcid` scripts for interactive development (notice that they are called outside of any dev shell):
1. `cd back && ./ghcid.sh` for backend.
2. `cd front && ./ghcid.sh` for frontend.
3. `cd game && ./ghcid.sh` for gameplay part.

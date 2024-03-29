with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    release-21-05.cabal-install
    pkgs.git
    pkgs.zlib
  ];

  shellHook = ''
    export PATH=./hls:$PATH # Run the command '$(nix-build build-hls.sh.nix)/bin/build-hls.sh' to install HLS in this directory
  '';
}

{ hls-version ? "2.4.0.0"
, exe-dir-expression ? "$(pwd)/hls"
, shellNixFile ? ./shell.nix
, nixpkgsFile ? ./nix/pkgs.nix
}:
let pkgs = (import nixpkgsFile).pkgs;
    nix = pkgs.nixVersions.nix_2_14;
    coreutils = pkgs.coreutils;
    realpath = "${coreutils}/bin/realpath";
in
pkgs.writeScriptBin "build-hls.sh" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i bash ${shellNixFile} --argstr nixpkgsFile ${nixpkgsFile}

    set -euxo pipefail

    EXE_DIR="${exe-dir-expression}"
    GHC_VERSION=$(ghc --numeric-version)

    cd "$(mktemp -d)"
    git clone https://github.com/haskell/haskell-language-server.git
    cd haskell-language-server
    git checkout ${hls-version}
    cabal build exe:haskell-language-server
    cabal install --installdir "$EXE_DIR" --overwrite-policy=always exe:haskell-language-server
    echo "Built haskell-language-server v${hls-version}"
    mv "$EXE_DIR/haskell-language-server" "$EXE_DIR/haskell-language-server-$GHC_VERSION"
    echo "Executables installed in $(realpath $EXE_DIR)"
''

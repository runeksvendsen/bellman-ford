{ nixpkgs ? (import ./nix/pkgs.nix).pkgs
, compiler ? "ghc92"
}:
let
  mutable-containers = nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix
    "mutable-containers"
    (mono-traversableSrc + "/mutable-containers")
    {};

  mono-traversableSrc = builtins.fetchGit {
    url = "https://github.com/runeksvendsen/mono-traversable.git";
    rev = "5bcff3e91337c071a7855be0691eae1bc91f35b5";
  };

  args =
    { mutable-containers = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
        (nixpkgs.pkgs.haskell.lib.overrideCabal mutable-containers)
        { };
    };
in
  nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "bellman-ford" ./. args

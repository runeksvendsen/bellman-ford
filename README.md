# Overview

[<img src="https://github.com/runeksvendsen/bellman-ford/actions/workflows/cabal-in-nix-shell.yml/badge.svg">](https://github.com/runeksvendsen/bellman-ford/actions?query=branch%3Amaster)

A translation of Sedgewick & Wayne's `BellmanFordSP` implementation to Haskell

# Development

The dependencies required to build this project are managed by Nix. To obtain a shell in which all required build dependencies are present, first [install Nix](https://nixos.org/download) and then run `nix-shell`.

## `haskell-language-server`

To build a version of haskell-language-server (HLS) that works with this project, run the following command:

```
$(nix-build build-hls.sh.nix)/bin/build-hls.sh
```

After running this command, run `nix-shell` to enter a shell in which the HLS executables are in the PATH. Running VS Code (with the Haskell extension installed) inside this shell should Just Work provided the VS Code setting `haskell.manageHLS` is set to `PATH`.

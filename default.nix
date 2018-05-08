{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
  with pkgs;
  let
    haskell-packages = haskell.packages.${compiler}.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (
          args // {
            enableLibraryProfiling = true;
          }
        );
      };
    };
    radix-tree = haskell-packages.callCabal2nix "dfinity-radix-tree" ./. {};
  in
  if pkgs.lib.inNixShell
  then stdenv.lib.overrideDerivation radix-tree.env (
    oldAttrs: {
      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ cabal-install stack ];
    }
  )
  else radix-tree

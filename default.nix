{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc782
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "binders";
  version = "0.1.0.0";
  src = ./.;
  hyperlinkSource = false;
  isLibrary = false;
  isExecutable = true;
})

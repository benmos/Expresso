{ pkgs ? import <latest> {} }:
 pkgs.lib.overrideDerivation (import ./build.nix { inherit pkgs; }).env (attrs: {
   buildInputs = [ pkgs.haskellPackages.cabal-install ] ++ attrs.buildInputs; # for 'cabal repl' etc.
 })


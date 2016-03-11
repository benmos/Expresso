{ pkgs ? import <latest> {} }:
let haskellPackages = pkgs.haskellngPackages.override {
      overrides = self: super: {
        expresso = self.callPackage ./. {};
      };
    };
 in haskellPackages.expresso

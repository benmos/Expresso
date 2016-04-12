{ mkDerivation, stdenv,
  ghcjs-dom,
  aeson, array, errors, mtl, text, time, transformers, wl-pprint-text
}:

mkDerivation (
let
  lib         = stdenv.lib;
  isWithin    = p: dirPath: lib.hasPrefix (toString dirPath) (toString p);
  cabalFilter = path: type: (let pathBaseName = baseNameOf path; in
                               !(lib.hasSuffix "~" pathBaseName) &&
                               !(lib.hasSuffix "#" pathBaseName) &&
                               !(lib.hasPrefix "." pathBaseName) &&
                               (
                                   pathBaseName == "Expresso.cabal" ||
                                   pathBaseName == "LICENSE"        ||
                                   pathBaseName == "Setup.lhs"      ||
                                   isWithin path ./src              ||
                                   # isWithin path ./static         ||
                                   false
                               )
                            );
in {
  pname = "Expresso";
  version = "1.0.0";

  # We White-list the files to use to in 'src' in order to avoid
  # spurious rebuilds. See eg [https://github.com/NixOS/nixpkgs/issues/3112]
  src = builtins.filterSource cabalFilter ./.;
  buildDepends = [
    # Others
    ghcjs-dom
    aeson array errors mtl text time transformers wl-pprint-text
  ];

  isLibrary = false;
  doHaddock = false;
  hyperlinkSource = false; # Don't waste time on Haddock
  enableSplitObjs = false;
  license = null;
})

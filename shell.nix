{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", withHoogle ? true }:
let
  inherit (nixpkgs) pkgs;
  pinnedUnstable =
    pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      rev = "c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38";
      sha256 = "1ak7jqx94fjhc68xh1lh35kh3w3ndbadprrb762qgvcfb8351x8v";
    };
  unstable = import pinnedUnstable {};
  ghcVersion = unstable.haskell.packages.${compiler};
  hspkgs =
    if withHoogle
       then
         ghcVersion.override {
           overrides = (self: super: {
             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
             ghcWithPackages = self.ghc.withPackages;
           });
         }
       else ghcVersion;

  origBuild = hspkgs.callPackage ./plaid.nix {};
  drv = unstable.haskell.lib.overrideCabal origBuild (drv: {
    libraryToolDepends = [
      unstable.stack
      unstable.hlint
      unstable.ghcid
      unstable.stdenv
      unstable.pkg-config
    ];
    librarySystemDepends = [ unstable.zlib ];
    license = unstable.stdenv.lib.licenses.bsd3;
    shellHook = ''
      '';
  });

in
  if pkgs.lib.inNixShell then drv.env else drv

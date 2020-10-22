{
  description = "dot emacs flake";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs-channels";
      ref = "nixpkgs-unstable";
    };
    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
    };
  };

  outputs = { self, ... }@inputs:
    let
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      emacs = pkgs.emacs.override { imagemagick = pkgs.imagemagickBig; };
      emacs-packages = import ./packages.nix { inherit pkgs; };
      emacs-final =
        (pkgs.emacsPackagesNgGen emacs).emacsWithPackages emacs-packages;
    in {
      packages.x86_64-linux.emacs = emacs-final;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.emacs;
    };
}

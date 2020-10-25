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

  outputs = { self, nixpkgs, emacs-overlay }:
    with import nixpkgs {
      system = "x86_64-linux";
      overlays = [ emacs-overlay.overlay ];
    };
    let
      emacs-packages = import ./packages.nix { inherit pkgs; };
      emacs-final =
        (pkgs.emacsPackagesNgGen pkgs.emacs).emacsWithPackages emacs-packages;
    in { defaultPackage.x86_64-linux = emacs-final; };
}

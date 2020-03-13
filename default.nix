{ pkgs ? import <nixpkgs> { } }:
let
  emacsWithPackages = (pkgs.emacsPackagesNgGen pkgs.emacs).emacsWithPackages;
  packages = import ./packages.nix { };
in emacsWithPackages packages

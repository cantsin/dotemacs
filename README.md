# .emacs.d

My emacs configuration; requires [Nix](https://nixos.org/nix/ "Nix").

Run `nix-build` for the derivation. Or, using [Home
Manager](https://github.com/rycee/home-manager/ "Home Manager"),

```
    programs.emacs = {
        enable = true;
        extraPackages = import ./packages.nix { inherit pkgs; };
    }
```

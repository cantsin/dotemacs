{ pkgs }:
epkgs:
(with epkgs.melpaPackages;
  [
    alchemist
    all-the-icons
    all-the-icons-dired
    all-the-icons-ivy
    arduino-mode
    autodisass-llvm-bitcode
    avy
    cargo
    cider
    company
    company-emacs-eclim
    company-ghc
    company-lsp
    counsel
    counsel-projectile
    dap-mode
    direnv
    dockerfile-mode
    doom-modeline
    easy-kill
    easy-kill-extras
    eclim
    esxml
    expand-region
    f
    flycheck
    flycheck-haskell
    flycheck-rust
    flymake-lua
    forge
    format-sql
    fsharp-mode
    gitconfig
    gitignore-mode
    glsl-mode
    go-mode
    handlebars-mode
    haskell-mode
    ht
    htmlize
    hydra
    idris-mode
    interleave
    irony
    ivy
    ivy-hydra
    ivy-rich
    jedi
    jinja2-mode
    js2-mode
    json-mode
    key-chord
    kotlin-mode
    kv
    ledger-mode
    lsp-mode
    lsp-ui
    magit
    magit-todos
    markdown-mode
    mc-extras
    merlin
    moe-theme
    multiple-cursors
    nix-mode
    nix-update
    nodejs-repl
    notmuch
    org-clock-convenience
    org-journal
    org-superstar
    pandoc-mode
    popup
    pretty-mode
    projectile
    racer
    racket-mode
    reason-mode
    restclient
    ripgrep
    rjsx-mode
    rustic
    rust-mode
    s
    scala-mode
    scss-mode
    session
    smart-compile
    smartparens
    smex
    stripe-buffer
    tagedit
    terraform-mode
    tide
    toml-mode
    tuareg
    typescript-mode
    typo
    use-package
    virtualenvwrapper
    wc-mode
    web-mode
    wgrep
    whitespace-cleanup-mode
    writegood-mode
    ws-butler
    yaml-mode
    zig-mode
  ] ++ [
    # overrides
    (epkgs.lua-mode.override (args: {
      melpaBuild = drv:
        args.melpaBuild (drv // {
          src = pkgs.fetchFromGitHub {
            owner = "immerrr";
            repo = "lua-mode";
            rev = "345ebfc1e236d9676e7e9f7364493785e7756348";
            sha256 = "sha256-m5Zy4u9tyAFAslBBMP/hYQWoLfqaX7xTrCOjHGeBoHs=";
            # date = 2020-09-21T19:45:07+02:00;
          };
        });
    }))
  ] ++ [ epkgs.orgPackages.org-plus-contrib epkgs.pdf-tools ])

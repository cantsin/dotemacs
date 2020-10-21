{ pkgs ? import <nixpkgs> { } }:
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
    deft
    diminish
    direnv
    dockerfile-mode
    doom-modeline
    easy-kill
    easy-kill-extras
    eclim
    elm-mode
    ember-mode
    epl
    erc-image
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
    package-build
    pallet
    pandoc-mode
    paradox
    pkg-info
    popup
    pretty-mode
    prodigy
    projectile
    puppet-mode
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
    solarized-theme
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
    # (epkgs.lua-mode.override (args: {
    #   melpaBuild = drv:
    #     args.melpaBuild (drv // {
    #       src = pkgs.fetchFromGitHub {
    #         owner = "immerrr";
    #         repo = "lua-mode";
    #         rev = "52cc3e465a2d35dbcbad8a87fd5fe548840f5822";
    #         sha256 = "1iw0z6dxd1nwjmlgy800xd2pgv40f798j831ca1hh3pbai5f84zm";
    #         # date = 2019-10-15T09:33:40+02:00;
    #       };
    #     });
    # }))
  ] ++ [
    epkgs.orgPackages.org-plus-contrib
    epkgs.pdf-tools
  ]
  # utilities
  ++ [ pkgs.shellcheck ])

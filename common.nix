# Home manager configuration shared across all machines.

{config, pkgs, ...}:

let
  # Extra packages.
  tygo = pkgs.callPackage ./pkgs/tygo.nix {};
  typescript-ls = pkgs.callPackage ./pkgs/typescript-ls.nix {};
in
{
  home.stateVersion = "22.05";
  programs.home-manager.enable = true;

  home.sessionPath = [
    "$HOME/.bin"
    "$HOME/.cargo/bin"
    "$HOME/.go/bin"
  ];

  home.sessionVariables = {
    EDITOR = "editor";
    PAGER = "cat"; # Works for now.
    JULIA_NUM_THREADS = "16";
  };

  home.file.".bin/editor" = {
    executable = true;
    source = ./scripts/editor;
  };

  home.shellAliases = {
    ec = "editor -nc";
    et = "editor -nw";
  };

  # Packages

  home.packages = with pkgs; [
    # Misc
    htop
    fortune
    vim
    coreutils
    cacert
    binutils
    git
    gnumake
    gnupg
    jq
    ripgrep
    tree
    docker
    gnupg
    gnugrep
    gnutar
    gnused
    syncthing
    tygo
    postgresql
    languagetool
    ispell
    qemu
    findutils
    plantuml

    # Find missing binaries.
    nix-index

    # Shells
    bash
    zsh

    # Linting stuff
    shellcheck
    golangci-lint

    # Dev utilities
    (pkgs.google-cloud-sdk.withExtraComponents
      ([pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin]))
    docker
    protobuf
    sqlc
    kubectl
    cloud-sql-proxy
    terraform

    # Python
    python311

    # Go
    go
    gopls
    gotools

    # Rust
    (fenix.beta.withComponents [
      "cargo"
      "clippy"
      "rust-src"
      "rustc"
      "rustfmt"
    ])
    rust-analyzer

    # Javascript/Typescript
    yarn
    nodejs-16_x
    nodePackages.typescript
    typescript-ls

    # Scheme
    guile

    # Clojure
    clojure

    # Common lisp
    sbcl

    # Elixir/erlang
    elixir
    erlang

    # Haskell
    haskell.compiler.ghc942
    ormolu

    # Fonts
    source-sans
    source-serif
    source-code-pro
  ];

  home.file.".emacs.d" = {
    source = ./emacs;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
  };

  programs.go = {
    enable = true;
    goPath = ".go";
  };

  programs.git = {
    enable = true;
    userEmail = "scsmithr@gmail.com";
    userName = "Sean Smith";
    signing = {
      key = "BA3E3A399960AD0D";
      signByDefault = true;
    };
    attributes = [
      "*.org diff=org"
      "*.md diff=md"
    ];
    ignores = [
      "worktree/"
      "vendor/"
      "node_modules/"
      ".log/"
      ".ccls-cache/"
    ];
    extraConfig = {
      core = {
        editor = "editor";
      };
      github = {
        user = "scsmithr";
      };
      init = {
        defaultBranch = "main";
      };
    };
  };

  programs.man = {
    enable = true;
    generateCaches = true;
  };

  # Shells

  programs.bash = {
    enable = true;
    # Be consistent so eshell regex can match this.
    initExtra = "PS1='[\\u@\\h \\W]\\$ '";
  };

  programs.zsh = {
    enable = true;
  };
}

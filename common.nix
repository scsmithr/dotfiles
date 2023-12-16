# Home manager configuration shared across all machines.

{config, pkgs, ...}:

let
  # Extra packages.
  tygo = pkgs.callPackage ./pkgs/tygo.nix {};
  goose = pkgs.callPackage ./pkgs/goose.nix {};
  d2 = pkgs.callPackage ./pkgs/d2.nix {};
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

    # Ensure system packages can be found by pkg-config.
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
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
    wget
    git
    gnumake
    gnupg
    jq
    ripgrep
    tree
    bat
    docker
    gnupg
    gnugrep
    gnutar
    gnused
    syncthing
    postgresql
    mysql
    sqlcmd # SQL Server cli
    qemu
    findutils
    plantuml
    unixtools.getopt
    cloc
    poppler_utils
    pkg-config
    openssl
    openssl.dev
    libiconv
    just
    neovim
    ffmpeg
    optipng
    imagemagick

    # PDF rendering
    # Stuff needed for nice pdf rendering using doc-view.
    ghostscript
    mupdf

    # Security
    tfsec
    doppler

    # Profiling
    samply

    # Speling
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # Shells
    bash
    zsh

    # Linting stuff
    shellcheck
    golangci-lint

    # Dev utilities
    (pkgs.google-cloud-sdk.withExtraComponents
      ([pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin]))
    awscli2
    azure-cli
    azure-storage-azcopy
    docker
    sqlc
    kubectl
    terraform
    skopeo
    gh
    d2

    # Protobuf
    protobuf
    protoc-gen-go
    protoc-gen-go-grpc

    # C/C++
    cmake
    doxygen
    clang-tools
    # TODO: clang and clangd

    # R
    (rWrapper.override { packages = with rPackages; [
                           ggplot2
                           dplyr
                           xts
                           RPostgres
                           DBI
                         ]; })

    # Python
    (python3.withPackages (ps: with ps; [
      numpy
      pandas
      pip
      virtualenv
      duckdb
      ipython
      pyarrow
      psycopg2
      rich
      pyspark
      requests-cache
      requests
    ]))
    poetry

    # Go
    go
    gopls
    gotools
    goose
    tygo

    # Rust
    (fenix.stable.withComponents [
      "cargo"
      "clippy"
      "rust-src"
      "rustc"
      "rustfmt"
    ])
    rust-analyzer-nightly

    # Javascript/Typescript
    yarn
    nodejs_20
    nodePackages.typescript
    typescript-ls
    esbuild

    # Scheme
    guile

    # Clojure
    clojure

    # Java
    jdk

    # Common lisp
    sbcl

    # Elixir/erlang
    elixir
    erlang

    # Haskell
    haskell.compiler.ghc942
    ormolu

    # Zig
    zig
    zls

    # Fonts
    source-sans
    source-serif
    source-code-pro
    fira-mono
    ibm-plex
    jetbrains-mono
    dejavu_fonts
    inconsolata
    mononoki
    cascadia-code
    victor-mono
    fantasque-sans-mono
    commit-mono
    go-font
    merriweather
    merriweather-sans

    # (iosevka.override {
    #   privateBuildPlan = builtins.readFile ./iosevka-custom.toml;
    #   set = "custom";
    # })
    # (iosevka.override {
    #   privateBuildPlan = builtins.readFile ./iosevka-sans.toml;
    #   set = "sans";
    # })
  ];

  home.file.".emacs.d" = {
    source = ./emacs;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29.pkgs.withPackages (epkgs: [
      epkgs.treesit-grammars.with-all-grammars
    ]);
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
      ".DS_Store"
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

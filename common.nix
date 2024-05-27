# Home manager configuration shared across all machines.

{config, pkgs, specialArgs, ...}:

let
  tygo = pkgs.buildGoModule {
    src = pkgs.fetchFromGitHub {
      owner = "gzuidhof";
      repo = "tygo";
      rev = "v0.2.4";
      sha256 = "sha256-pH60K7F8SRBBZIrog7AN/fa7ES/OQ5u9/0vbCEoTJq8=";
    };

    pname = "tygo";
    name = "tygo";

    vendorHash = "sha256-Suwo9xyj34IEBqu328EEl8GCS9QthFWnSKlU4gRUMAU=";
  };

  goose = pkgs.buildGoModule {
    src = pkgs.fetchFromGitHub {
      owner = "pressly";
      repo = "goose";
      rev = "v3.7.0";
      sha256 = "sha256-2T+Mb9SCsYsrD2FqddpszVjcaSdJcLn3RnJMloxI4xQ=";
    };

    pname = "goose";
    name = "goose";
    # Tries to run tests with docker containers.
    doCheck = false;

    vendorHash = "sha256-MiPbUq3iiCSZRG4FeC1NAny2BflBnlTxq4Id5Xc3Kxo=";
  };

  cloud-provider-kind = pkgs.buildGoModule {
    src = pkgs.fetchFromGitHub {
      owner = "kubernetes-sigs";
      repo = "cloud-provider-kind";
      rev = "v0.1.0";
      sha256 = "sha256-PWdw0B4RwrJfqq4tTBKGZXkt3zcxEa6p7j/0UCLhDnA=";
    };

    pname = "cloud-provider-kind";
    name = "cloud-provider-kind";

    vendorHash = null;
  };

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
    cacert
    cloc
    coreutils
    docker
    ffmpeg
    findutils
    git
    gnugrep
    gnumake
    gnupg
    gnupg
    gnused
    gnutar
    htop
    imagemagick
    jq
    just
    neovim
    optipng
    pandoc
    poppler_utils
    qemu
    ripgrep
    texlive.combined.scheme-full
    tree
    unixtools.getopt
    vim
    wget
    xz

    # Random build bullshit
    libiconv

    # Needed for compiling psycopg2 from source since it doesn't ship a
    # wheel for mac.
    openssl
    openssl.dev

    # SQL clients
    postgresql
    mysql
    sqlcmd # SQL Server cli
    clickhouse

    # PDF rendering
    # Stuff needed for nice pdf rendering using doc-view.
    mupdf

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

    # Link checking
    lychee

    # Cloud utilities
    (pkgs.google-cloud-sdk.withExtraComponents
      ([pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin]))
    awscli2
    azure-cli
    azure-storage-azcopy

    # k8s
    kubectl
    kubernetes-helm
    kind
    cloud-provider-kind

    # Other cloud stuff
    terraform

    # Security
    tfsec
    doppler

    # C/C++
    cmake
    doxygen
    clang-tools

    # Zig
    zig
    zls

    # Protobuf
    protobuf
    protoc-gen-go
    protoc-gen-go-grpc

    # Flatbuffers
    flatbuffers

    # Rust
    (with fenix; combine [
      stable.cargo
      stable.clippy
      stable.rust-src
      stable.rustc
      latest.rustfmt
      latest.miri
      targets.wasm32-unknown-unknown.stable.rust-std
    ])
    rust-analyzer-nightly
    cargo-insta

    # Wasm
    wasm-pack

    # Go
    go
    gopls
    gotools
    golangci-lint
    goose
    tygo
    sqlc

    # Javascript/Typescript
    yarn
    nodejs_20
    nodePackages.typescript
    nodePackages.typescript-language-server
    esbuild

    # Java
    jdk21_headless

    # Python with some common libs.
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
      dbt

      # Languge server (pylsp)
      python-lsp-server
      pyls-flake8
      flake8
    ]))
    poetry
    ruff

    # For R examples.
    (rWrapper.override
      { packages = with rPackages; [
          ggplot2
          dplyr
          xts
          RPostgres
          DBI
        ]; })

    # Fonts
    fira-mono
    ibm-plex
    inconsolata
    iosevka
    jetbrains-mono
    merriweather
    merriweather-sans
    open-sans
    source-code-pro
    source-sans
    source-serif
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
      ".envrc"
      ".direnv/"
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

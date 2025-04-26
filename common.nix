# Home manager configuration shared across all machines.

{config, pkgs, specialArgs, ...}:

let
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
    tree
    unixtools.getopt
    vim
    wget
    xz

    # Github
    gh

    # Random build bullshit
    libiconv

    # Needed for compiling psycopg2 from source since it doesn't ship a
    # wheel for mac.
    openssl
    openssl.dev

    # SQL clients
    postgresql
    mariadb
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

    # Linting stuff
    shellcheck

    # Link checking
    # lychee

    # Cloud utilities
    (pkgs.google-cloud-sdk.withExtraComponents
      ([pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin]))
    # awscli2
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

    # Protobuf
    protobuf
    protoc-gen-go
    protoc-gen-go-grpc

    # Flatbuffers
    flatbuffers

    # Thrift
    thrift

    # Rust
    rustup

    # Wasm
    wasm-pack
    wasm-bindgen-cli

    # Go
    go
    gopls
    gotools
    golangci-lint

    # Javascript/Typescript
    yarn
    nodejs_20
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.vercel
    esbuild

    # Java
    jdk21_headless

    # Python with some common libs.
    (python3.withPackages (ps: with ps; [
      numpy
      pandas
      pip
      virtualenv
      pyarrow
      psycopg2
      pyspark
      requests-cache
      requests

      # Languge server (pylsp)
      python-lsp-server
      pyls-flake8
      flake8
    ]))
    poetry
    ruff
    maturin
    pyenv

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
    package = (
      # https://github.com/NixOS/nixpkgs/issues/395169
      pkgs.emacs.override {
        withNativeCompilation = false;
      }
    )
    .pkgs.withPackages (epkgs: [
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

# Home manager configuration shared across all machines.

{config, pkgs, specialArgs, ...}:

let
  rust-dev-packages = specialArgs.rust-dev-packages;
  python-dev-packages = specialArgs.python-dev-packages;
  go-dev-packages = specialArgs.go-dev-packages;
  ts-dev-packages = specialArgs.ts-dev-packages;
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
    htop
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
    gnupg
    gnugrep
    gnutar
    gnused
    qemu
    findutils
    unixtools.getopt
    cloc
    poppler_utils
    neovim
    ffmpeg
    optipng
    imagemagick
    docker

    # For psql
    postgresql

    # PDF rendering
    # Stuff needed for nice pdf rendering using doc-view.
    ghostscript
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

    # Cloud utilities
    (pkgs.google-cloud-sdk.withExtraComponents
      ([pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin]))
    awscli2
    azure-cli
    azure-storage-azcopy
    kubectl

    # C/C++
    cmake
    doxygen
    clang-tools

    # Zig
    zig
    zls

    # Protobuf
    protobuf

    # Fonts
    source-sans
    source-serif
    source-code-pro
    fira-mono
    ibm-plex
    jetbrains-mono
    merriweather
    merriweather-sans
  ]
  ++ rust-dev-packages
  ++ python-dev-packages
  ++ go-dev-packages
  ++ ts-dev-packages;

  home.file.".emacs.d" = {
    source = ./emacs;
    recursive = true;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
    config = {
      whitelist = {
        prefix = ["~/Code/github.com/glaredb/"];
      };
    };
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

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

  home.username = "sean";
  home.homeDirectory = "/Users/sean";

  # Use the exposed script since docker desktop will only make explicitly
  # exposed ports available.
  home.file.".bin/postgres-scratch" = {
    executable = true;
    source = ./scripts/postgres-scratch-exposed;
  };

  # Increase max number of open files per shell.
  #
  # Mac has a default of 256, and I very often hit this with emacs.
  programs.bash.bashrcExtra = "ulimit -Sn 8192";
  programs.zsh.initContent = "ulimit -Sn 8192";

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
    helix
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

    # Take my job
    claude-code
    codex

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
    lychee

    # Cloud utilities
    (pkgs.google-cloud-sdk.withExtraComponents
      ([pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin]))
    awscli2

    # k8s
    kubectl
    kustomize
    kubernetes-helm
    kind
    cloud-provider-kind

    # Security
    doppler

    # C/C++
    cmake
    doxygen
    ninja

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
    nodejs
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
    uv

    # Fonts
    fira-mono
    ibm-plex
    inconsolata
    iosevka
    # jetbrains-mono # Build error
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
    package = let
      liquidGlassIcons = pkgs.fetchFromGitHub {
        owner = "jimeh";
        repo = "emacs-liquid-glass-icons";
        rev = "main";
        sha256 = "09ymkmxbr6imj6pl5wnfnwkwg8z8r4rk37r683mmbbfan2h30q6f";
      };

      # https://github.com/NixOS/nixpkgs/issues/395169
      baseEmacs = pkgs.emacs.override {
        withNativeCompilation = false;
      };

      emacsWithIcons = baseEmacs.overrideAttrs (old: {
        postInstall = (old.postInstall or "") + ''
          # For macOS 26+ - use Assets.car approach
          if [ -d $out/Applications/Emacs.app/Contents/Resources ]; then
            echo "Installing liquid glass icons..."
            cp -v ${liquidGlassIcons}/Resources/Assets.car \
               $out/Applications/Emacs.app/Contents/Resources/Assets.car

            # Update Info.plist to set CFBundleIconName using PlistBuddy
            plistFile=$out/Applications/Emacs.app/Contents/Info.plist
            /usr/libexec/PlistBuddy -c "Add :CFBundleIconName string EmacsLG1" "$plistFile" 2>/dev/null || \
            /usr/libexec/PlistBuddy -c "Set :CFBundleIconName EmacsLG1" "$plistFile"

            echo "Icon installation complete"
          fi
        '';
      });
    in (pkgs.emacsPackagesFor emacsWithIcons).withPackages (epkgs: [
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

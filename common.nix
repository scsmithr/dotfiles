{config, pkgs, ...}:
{
  home.username = "sean";
  home.homeDirectory = "/home/sean";

  home.stateVersion = "22.05";
  programs.home-manager.enable = true;

  home.sessionPath = [
    "$HOME/.bin/"
    "$HOME/.cargo/bin"
    "$HOME/\${config.programs.go.goPath}/bin"
    # Stack installs binaries here. Needed since I'm using stack to manage
    # xmonad.
    "$HOME/.local/bin"
  ];

  home.sessionVariables = {
    EDITOR = "editor";
    JULIA_NUM_THREADS = "16";
  };

  home.file.".bin/editor" = {
    executable = true;
    source = ./scripts/editor;
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

    # Google cloud
    google-cloud-sdk

    # Go
    go
    gopls

    # Rust
    cargo
    rustc
    rustfmt
    rust-analyzer

    # Javascript/Typescript
    yarn
    nodejs-16_x
  ];

  programs.firefox = {
    enable = true;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      font.size = 11;
    };
  };

  home.file.".emacs.d" = {
    source = ./emacs;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
  };

  programs.bash = {
    enable = true;
    # Be consistent so eshell regex can match this.
    bashrcExtra = "PS1='[\u@\h \W]\$ '";
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
}

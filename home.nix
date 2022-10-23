{config, pkgs, ...}:
{
  home.username = "sean";
  home.homeDirectory = "/home/sean";

  home.stateVersion = "22.05";
  programs.home-manager.enable = true;

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

    # Go
    go
    gopls

    # Rust
    cargo
    rustc
    rustfmt
    rust-analyzer
  ];

  programs.alacritty = {
    enable = true;
    settings = {
      font.size = 11;
    };
  };

  # Emacs
  home.file.".emacs.d" = {
    source = ./emacs;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
  };
}

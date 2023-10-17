# Home manager configuration specific to my Darwin machines.

{config, lib, pkgs, ...}:
{
  home.username = "sean";
  home.homeDirectory = "/Users/sean";

  # Use the exposed script since docker desktop will only make explicitly
  # exposed ports available.
  home.file.".bin/postgres-scratch" = {
    executable = true;
    source = ./scripts/postgres-scratch-exposed;
  };

  home.file.".cargo/config.toml" = {
    text = ''
    [env]
    PYO3_PYTHON = "/Users/sean/.nix-profile/bin/python3"
    '';
  };

  # Increase max number of open files per shell.
  #
  # Mac has a default of 256, and I very often hit this with emacs.
  programs.bash.bashrcExtra = "ulimit -Sn 8192";
  programs.zsh.initExtra = "ulimit -Sn 8192";
}

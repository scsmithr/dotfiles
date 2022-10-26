# Home manager configuration specific to my Linux machines.

{config, lib, pkgs, ...}:
{
  home.username = "sean";
  home.homeDirectory = "/home/sean";

  home.file.".bin/postgres-scratch" = {
    executable = true;
    source = ./scripts/postgres-scratch;
  };

  home.packages = with pkgs; [
    xorg.xmodmap
    gnome.gnome-terminal
  ];
}

# Home manager configuration specific to my Linux machines.

{config, lib, pkgs, ...}:
{
  home.username = "sean";
  home.homeDirectory = "/home/sean";

  home.packages = with pkgs; [
    xorg.xmodmap
    gnome.gnome-terminal
  ];
}

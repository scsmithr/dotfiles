# Home manager configuration specific to my Linux machines.

{config, pkgs, ...}:
{
  home.username = "sean";
  home.homeDirectory = "/home/sean";

  programs.xmobar = {
    enable = true;
  };

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./linux/xmonad.hs;
  };
}

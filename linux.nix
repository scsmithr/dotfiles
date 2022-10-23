# Home manager configuration specific to my Linux machines.

{config, lib, pkgs, ...}:
{
  home.username = "sean";
  home.homeDirectory = "/home/sean";

  # idk how to loop

  home.file.".bin/bri" = {
    executable = true;
    source = ./linux/scripts/bri;
  };

  home.file.".bin/dpi" = {
    executable = true;
    source = ./linux/scripts/dpi;
  };

  home.file.".bin/essid" = {
    executable = true;
    source = ./linux/scripts/essid;
  };

  home.file.".bin/lock" = {
    executable = true;
    source = ./linux/scripts/lock;
  };

  home.file.".bin/notify" = {
    executable = true;
    source = ./linux/scripts/notify;
  };

  home.file.".bin/screenshot" = {
    executable = true;
    source = ./linux/scripts/screenshot;
  };

  home.file.".bin/toggle-redshift" = {
    executable = true;
    source = ./linux/scripts/toggle-redshift;
  };

  home.file.".bin/vol" = {
    executable = true;
    source = ./linux/scripts/vol;
  };

  home.file.".status/status.scm" = {
    source = ./linux/status.scm;
  };

  programs.xmobar = {
    enable = true;
    extraConfig = ''
        Config {
            font = "xft:Source Sans Pro:semibold:size=11",
            bgColor = "#36373d",
            fgColor = "#ababb4",
            border = FullB,
            borderColor = "#26272d",
            borderWidth = 1,
            position = Top,
            allDesktops = False,
            lowerOnStart = True,
            hideOnStart = False,
            persistent = True,

            commands = [
                Run Date "%a, %b %d %I:%M%P" "date" 10,

                Run CommandReader "STATUS_SECTION_COLOR=#828282 guile -e main -s ~/.status/status.scm" "status",

                Run StdinReader
            ],
            sepChar = "%",
            alignSep = "}{",
            template = " %StdinReader% }  %date%  { %status% "
                                                  }
    '';
  };

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./linux/xmonad.hs;
  };
}

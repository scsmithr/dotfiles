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
}

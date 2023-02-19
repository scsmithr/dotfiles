{
  description = "Home Manager configuration for Sean";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Rust toolchain
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Pick different emacs sources.
    emacs-overlay = {
      url = "github:Nix-Community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Emacs 29 source.
    emacs29-src = {
      url = "github:emacs-mirror/emacs/emacs-29";
      flake = false;
    };
  };

  outputs = { nixpkgs, home-manager, fenix, emacs-overlay, emacs29-src, ... }:
    let
      mkHome = system: {modules}: (
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            inherit system;
            config = {allowUnfree = true;};
            overlays = [
              fenix.overlays.default
              emacs-overlay.overlays.emacs
              # Emacs 29
              (final: prev: {
                emacs29 = prev.emacsGit.overrideAttrs (old: {
                  name = "emacs29";
                  version = emacs29-src.shortRev;
                  src = emacs29-src;
                });
              })
            ];
          };
          inherit modules;
        }
      );
    in {
      homeConfigurations.sean-darwin = mkHome "aarch64-darwin" {
        modules = [
          ./darwin.nix
          ./common.nix
        ];
      };
      homeConfigurations.sean-linux = mkHome "x86_64-linux" {
        modules = [
          ./linux.nix
          ./common.nix
        ];
      };
    };
}

{
  description = "Home Manager configuration for Sean";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Stable nixpkgs.
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.05";

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

  outputs = { nixpkgs, nixpkgs-stable, home-manager, fenix, emacs-overlay, emacs29-src, ... }:
    let

      mkHome = system: {modules}: (
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              permittedInsecurePackages = [
                "openssl-1.1.1u" # Needed for spark
                "nodejs-16.20.1" # Needed for iosevka.
              ];
            };
            overlays = [
              fenix.overlays.default
              emacs-overlay.overlays.emacs
              # Emacs 29
              (final: prev: {
                emacs29 = prev.emacs-git.overrideAttrs (old: {
                  name = "emacs29";
                  version = emacs29-src.shortRev;
                  src = emacs29-src;
                });
              })
              # GPG, 2.4.1 causes emacs to hang when saving files. 2.4.0
              # (version on stable) does not hang.
              (final: prev:
                let
                  stable = import nixpkgs-stable { inherit system; };
                in {
                  gnupg = stable.gnupg;
                }
              )
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

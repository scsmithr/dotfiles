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
  };

  outputs = { nixpkgs, nixpkgs-stable, home-manager, fenix, ... }@inputs:
    let
      systemConfs = [
        {
          name = "sean-darwin";
          system = "aarch64-darwin";
          modules = [./common.nix ./darwin.nix];
        }
        {
          name = "sean-linux";
          system = "x86_64-linux";
          modules = [./common.nix ./linux.nix];
        }
      ];

      mkPkgs = system: import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
        overlays = [
          fenix.overlays.default
          # GPG, 2.4.1 causes emacs to hang when saving files. 2.4.0
          # (version on stable) does not hang.
          (final: prev:
            let
              stable = import nixpkgs-stable { inherit system; };
            in {
              gnupg = stable.gnupg;
            }
          )

          # Don't create a desktop entry for mupdf.
          (final: prev:
            let
              mupdf = prev.mupdf.overrideAttrs (attrs: {
                desktopItems = [];
              });
            in {
              mupdf = mupdf;
            }
          )
        ];
      };

      mkHome = {name, system, modules}:
        { name = name;
          value = home-manager.lib.homeManagerConfiguration {
            inherit modules;
            pkgs = mkPkgs system;
          };
        };

      homeConfs = map mkHome systemConfs;
    in {
      homeConfigurations = builtins.listToAttrs homeConfs;
    };
}

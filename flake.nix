{
  description = "Home Manager configuration for Sean";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Rust toolchain
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, fenix, ... }@inputs:
    let
      systemConfs = [
        {
          name = "sean-darwin";
          system = "aarch64-darwin";
          modules = [./common.nix];
        }
      ];

      mkPkgs = system: import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
        overlays = [
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

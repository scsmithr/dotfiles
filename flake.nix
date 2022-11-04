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
  };

  outputs = { nixpkgs, home-manager, fenix, ... }:
    let
      mkHome = system: {modules}: (
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            inherit system;
            config = {allowUnfree = true;};
            overlays = [fenix.overlay];
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

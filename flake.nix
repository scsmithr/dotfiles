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

      mkHome = system: {modules}: (
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
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
            ];
          };
          extraSpecialArgs = {
            rust-dev-packages = inputs.dev-glaredb.dev.${system}.rust-packages;
            python-dev-packages = inputs.dev-glaredb.dev.${system}.python-packages;
            go-dev-packages = inputs.dev-cloud.dev.${system}.go-packages;
            ts-dev-packages = inputs.dev-cloud.dev.${system}.ts-packages;
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

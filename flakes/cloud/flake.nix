# Dev env for cloud
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, flake-utils, fenix}:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
            };
          };

          tygo = pkgs.buildGoModule {
            src = pkgs.fetchFromGitHub {
              owner = "gzuidhof";
              repo = "tygo";
              rev = "v0.2.4";
              sha256 = "sha256-pH60K7F8SRBBZIrog7AN/fa7ES/OQ5u9/0vbCEoTJq8=";
            };

            pname = "tygo";
            name = "tygo";

            vendorHash = "sha256-Suwo9xyj34IEBqu328EEl8GCS9QthFWnSKlU4gRUMAU=";
          };

          goose = pkgs.buildGoModule {
            src = pkgs.fetchFromGitHub {
              owner = "pressly";
              repo = "goose";
              rev = "v3.7.0";
              sha256 = "sha256-2T+Mb9SCsYsrD2FqddpszVjcaSdJcLn3RnJMloxI4xQ=";
            };

            pname = "goose";
            name = "goose";
            # Tries to run tests with docker containers.
            doCheck = false;

            vendorHash = "sha256-MiPbUq3iiCSZRG4FeC1NAny2BflBnlTxq4Id5Xc3Kxo=";
          };

          tsBuildInputs = with pkgs; [
            yarn
            nodejs_20
            nodePackages.typescript
            nodePackages.typescript-language-server
            esbuild
          ];

          goBuildInputs = with pkgs; [
            # Go
            go
            gopls
            gotools
            golangci-lint
          ];

          buildInputs = with pkgs; [
            # Other go stuff
            goose
            tygo
            sqlc

            # Other cloud stuff
            terraform

            # Security
            tfsec
            doppler

            # Protobuf
            protobuf
            protoc-gen-go
            protoc-gen-go-grpc
          ]
          ++ goBuildInputs
          ++ tsBuildInputs;
        in
          {
            devShells.default = pkgs.mkShell {
              inherit buildInputs;

              ALLOW_LOCALHOST_ACCESS = true;
              GLARE_DEV_HOST = "localhost";
            };

            dev = {
              go-packages = goBuildInputs;
              ts-packages = tsBuildInputs;
            };
          }
      );
}

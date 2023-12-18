# Dev env for glaredb
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Rust toolchain
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, flake-utils, fenix, ...}:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          rust = fenix.packages.${system};

          rustBuildInputs = [
            (rust.stable.withComponents [
              "cargo"
              "clippy"
              "rust-src"
              "rustc"
              "rustfmt"
            ])
            rust.rust-analyzer
          ];

          # Python with some common libs.
          pythonBuildInputs = with pkgs; [
            (python3.withPackages (ps: with ps; [
              numpy
              pandas
              pip
              virtualenv
              duckdb
              ipython
              pyarrow
              psycopg2
              rich
              pyspark
              requests-cache
              requests
            ]))
            poetry
          ];

          # For R examples.
          rBuildInputs = with pkgs; [
            (rWrapper.override
              { packages = with rPackages; [
                  ggplot2
                  dplyr
                  xts
                  RPostgres
                  DBI
                ]; })
          ];

          buildInputs = with pkgs; [
            just
            protobuf

            # SQL clients
            postgresql
            mysql
            sqlcmd # SQL Server cli

            # Needed for compiling psycopg2 from source since it doesn't ship a
            # wheel for mac.
            openssl
            openssl.dev
          ]
          ++ rustBuildInputs
          ++ pythonBuildInputs
          ++ rBuildInputs
          ++ (pkgs.lib.optional pkgs.stdenv.isDarwin [
            # Packages required to build on mac.
            pkgs.libiconv
            pkgs.darwin.apple_sdk.frameworks.Security
            pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
          ]);

        in
          {
            devShells.default = pkgs.mkShell {
              inherit buildInputs;
            };

            dev = {
              rust-packages = rustBuildInputs;
              python-packages = pythonBuildInputs;
              r-packages = rBuildInputs;
            };
          }
      );
}

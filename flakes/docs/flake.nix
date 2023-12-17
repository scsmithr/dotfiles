# Dev env docs site
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
          };


          buildInputs = with pkgs; [
            bundler
            rubyPackages.sassc
          ];
        in
          {
            devShells.default = pkgs.mkShell {
              inherit buildInputs;
            };

            dev = {};
          }
      );
}

# Dev env website
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

          typescript-ls = pkgs.symlinkJoin {
            name = "typescript-language-server";
            paths = [ pkgs.nodePackages.typescript-language-server ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
                wrapProgram $out/bin/typescript-language-server \
                    --add-flags --tsserver-path=${pkgs.nodePackages.typescript}/lib/node_modules/typescript/lib/
            '';
          };

          buildInputs = with pkgs; [
            yarn
            nodejs_20
            nodePackages.typescript
            typescript-ls
            esbuild
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

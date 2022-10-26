{ pkgs }:

pkgs.symlinkJoin {
  name = "typescript-language-server";
  paths = [ pkgs.nodePackages.typescript-language-server ];
  buildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
      wrapProgram $out/bin/typescript-language-server \
        --add-flags --tsserver-path=${pkgs.nodePackages.typescript}/lib/node_modules/typescript/lib/
    '';
}

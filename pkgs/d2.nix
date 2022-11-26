{ pkgs }:

pkgs.buildGoModule {
  src = pkgs.fetchFromGitHub {
    owner = "terrastruct";
    repo = "d2";
    rev = "b54e376ff41bda84b436d65c16bdbe1f7919444a";
    sha256 = "sha256-ID/PybiZAx2S25nQvEVZN2OlVYcdwXLUXFRVcYJ/dDk=";
  };

  pname = "d2";
  name = "d2";

  # Go 1.19 adds an extra field to the URL type, which causes some tests to fail
  # since it check against output generated with Go 1.18.
  doCheck = false;

  vendorSha256 = "sha256-/BEl4UqOL4Ux7I2eubNH2YGGl4DxntpI5WN9ggvYu80=";
}

{ pkgs }:

pkgs.buildGoModule {
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

  vendorSha256 = "sha256-MiPbUq3iiCSZRG4FeC1NAny2BflBnlTxq4Id5Xc3Kxo=";
}

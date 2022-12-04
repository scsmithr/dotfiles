{ pkgs }:

pkgs.buildGoModule {
  src = pkgs.fetchFromGitHub {
    owner = "terrastruct";
    repo = "d2";
    rev = "a4e66f658a6f6a95749d37978521007d18e55a47";
    sha256 = "sha256-p25BJ7c4cFDTc6943kg5Vq9NHzYBG7/eQNLTkyk7Ddg=";
  };

  pname = "d2";
  name = "d2";

  # Go 1.19 adds an extra field to the URL type, which causes some tests to fail
  # since it check against output generated with Go 1.18.
  doCheck = false;

  vendorSha256 = "sha256-L6ivdbpwve3hYrhoDElExcu9MSQKbtC3jfuTXHdt1vE=";
}

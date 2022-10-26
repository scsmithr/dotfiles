{ pkgs }:

pkgs.buildGoModule {
  src = pkgs.fetchFromGitHub {
    owner = "gzuidhof";
    repo = "tygo";
    rev = "v0.2.4";
    sha256 = "sha256-pH60K7F8SRBBZIrog7AN/fa7ES/OQ5u9/0vbCEoTJq8=";
  };

  pname = "tygo";
  name = "tygo";

  vendorSha256 = "sha256-Suwo9xyj34IEBqu328EEl8GCS9QthFWnSKlU4gRUMAU=";
}

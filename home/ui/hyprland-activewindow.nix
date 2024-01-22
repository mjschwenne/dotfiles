{
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "hyprland-activewindow";
  version = "v0.4.4";

  src = fetchFromGitHub {
    owner = "FieldofClay";
    repo = pname;
    rev = version;
    sha256 = "sha256-DaQrVXrmDr1v/nECTVMfTrIgFe7j1dNLLGCjYeBfRpU=";
  };

  cargoSha256 = "sha256-r2oaXI9+Wfr2e6YnSu39tz8qfsmyrFGs4/0Zo/hjj1U=";
}

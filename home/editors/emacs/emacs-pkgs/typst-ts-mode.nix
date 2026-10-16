{
  trivialBuild,
  fetchFromGitea,
}:
trivialBuild {
  pname = "typst-ts-mode";
  version = "unstable-2026-10-15";
  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "meow_king";
    repo = "typst-ts-mode";
    rev = "ee17b331ebfa8d8b7c2f54a1c39ab3d76e134706";
    hash = "sha256-RFWr/OOgugcRm6PqPt+eWg/Rnm5fX7k4TjiaB+6H7XI=";
  };
}

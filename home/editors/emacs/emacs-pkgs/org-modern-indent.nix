{
  trivialBuild,
  fetchFromGitHub,
}:
trivialBuild {
  pname = "org-modern-indent";
  version = "unstable-2026-10-15";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "org-modern-indent";
    rev = "86bd83ee1ad95f123810eb3b116beb543db1960a";
    sha256 = "sha256-vQzYk5qejCBehpbxkMceOMsmeLyjnAstpezZw/ZR1jQ=";
  };
}

{
  trivialBuild,
  fetchFromGitea,
}:
trivialBuild {
  pname = "typst-ts-mode";
  version = "unstable-2025-10-01";
  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "meow_king";
    repo = "typst-ts-mode";
    rev = "7c2ef0d5bd2b5a8727fe6d00938c47ba562e0c94";
    hash = "sha256-D+QEfEYlxJICcdUCleWpe7+HxePLSSmV7zAwvyTL0+Q=";
  };
}

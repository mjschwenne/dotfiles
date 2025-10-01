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
    rev = "fa03e477dfc57ea498ef9a624b2ec7bf655dc25c";
    hash = "sha256-fso39K3MvF41M5LUzsl++GkR9+yAiJed4mxjKBqkoGE=";
  };
}

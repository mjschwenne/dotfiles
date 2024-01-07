{
  config,
  pkgs,
  fetchFromGitHub,
  ...
}: final: prev: {
  mako = prev.mako.overrideAttrs (old: rec {
    version = "1.8.0";
    pname = "mako";
    src = fetchFromGitHub {
      owner = "emersion";
      repo = pname;
      rev = "v${version}";
      sha256 = "sha256-sUFMcCrc5iNPeAmRbqDaT/n8OIlFJEwJTzY1HMx94RU=";
    };
  });
}

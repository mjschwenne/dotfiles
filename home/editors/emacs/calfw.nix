{
  trivialBuild,
  fetchFromGitHub,
  howm,
}:
trivialBuild rec {
  pname = "calw";
  version = "main-27-01-2024";
  src = fetchFromGitHub {
    owner = "haji-ali";
    repo = "emacs-calfw";
    rev = "bc99afee611690f85f0cd0bd33300f3385ddd3d3";
    hash = "sha256-0xMII1KJhTBgQ57tXJks0ZFYMXIanrOl9XyqVmu7a7Y=";
  };

  propagatedUserEnvPkgs = [howm];
  buildInputs = propagatedUserEnvPkgs;
}

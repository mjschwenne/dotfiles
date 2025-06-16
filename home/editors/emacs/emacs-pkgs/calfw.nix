{
  trivialBuild,
  fetchFromGitHub,
  howm,
}:
trivialBuild rec {
  pname = "calw";
  version = "unstable-2025-05-02";
  src = fetchFromGitHub {
    owner = "haji-ali";
    repo = "emacs-calfw";
    rev = "de99e8e848ee03811388f433f7eb0400976b791d";
    sha256 = "1gdpym3jggx2lgrj4i6g2l9bzw33cl79jk3km62ykzpnrqlfpvjs";
  };

  propagatedUserEnvPkgs = [howm];
  buildInputs = propagatedUserEnvPkgs;
}

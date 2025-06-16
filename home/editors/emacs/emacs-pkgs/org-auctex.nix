{
  trivialBuild,
  fetchFromGitHub,
  auctex,
}:
trivialBuild rec {
  pname = "org-auctex";
  version = "unstable-2022-04-02";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "org-auctex";
    rev = "e1271557b9f36ca94cabcbac816748e7d0dc989c";
    sha256 = "1z02vfimmkrl2422rmk63sgfgg9hryh556h3sz093az74v1j3h3h";
  };

  propagatedUserEnvPkgs = [auctex];
  buildInputs = propagatedUserEnvPkgs;
}

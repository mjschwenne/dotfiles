{ trivialBuild
, fetchFromGitHub
, auctex
,
}:
trivialBuild rec {
  pname = "org-auctex";
  version = "main-2023-04-03";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "org-auctex";
    rev = "e1271557b9f36ca94cabcbac816748e7d0dc989c";
    hash = "sha256-cMAhwybnq5HA1wOaUqDPML3nnh5m1iwEETTPWqPbAvw=";
  };

  propagatedUserEnvPkgs = [ auctex ];
  buildInputs = propagatedUserEnvPkgs;
}

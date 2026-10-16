{
  trivialBuild,
  fetchFromGitHub,
  org-ql,
  persist,
}:
trivialBuild rec {
  pname = "org-timeblock";
  version = "unstable-2026-10-15";
  src = fetchFromGitHub {
    owner = "ichernyshovvv";
    repo = "org-timeblock";
    rev = "e61e5734b49f933ed178029f804a0499f3308e1e";
    sha256 = "sha256-NOc45zpfvia61YpLiggP/YgJeLttNAd/Cdeh1CfxGdw=";
  };

  propagatedUserEnvPkgs = [
    org-ql
    persist
  ];
  buildInputs = propagatedUserEnvPkgs;
}

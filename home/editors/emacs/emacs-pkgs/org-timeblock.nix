{
  trivialBuild,
  fetchFromGitHub,
  org-ql,
  persist,
}:
trivialBuild rec {
  pname = "org-timeblock";
  version = "unstable-2024-10-27";
  src = fetchFromGitHub {
    owner = "ichernyshovvv";
    repo = "org-timeblock";
    rev = "e61e5734b49f933ed178029f804a0499f3308e1e";
    sha256 = "1p0ry4kx98fp15zhfd3dpdw0k27x1w48ljwasnx2dgjz7bkkirrl";
  };

  propagatedUserEnvPkgs = [org-ql persist];
  buildInputs = propagatedUserEnvPkgs;
}

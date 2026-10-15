{
  trivialBuild,
  fetchFromGitHub,
  consult,
}:
trivialBuild rec {
  pname = "consult-ripfd";
  version = "unstable-2026-10-13";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "consult-ripfd";
    rev = "f64ae7a4b30c605ec839b73d8e0768a73810a7de";
    hash = "sha256-sPb8PNhJCQ13CtFj/7JELU5ZsxwDs6+pMnxBRyvDcOA=";
  };
  propagatedUserEnvPkgs = [ consult ];
  buildInputs = propagatedUserEnvPkgs;
}

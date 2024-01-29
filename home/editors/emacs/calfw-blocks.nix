{
  trivialBuild,
  fetchFromGitHub,
  calfw,
  calfw-org,
}:
trivialBuild rec {
  pname = "calfw-blocks";
  version = "main-27-01-2024";
  src = fetchFromGitHub {
    owner = "ml729";
    repo = "calfw-blocks";
    rev = "0fe829035ffa491c3f2610f05a7f1ec936a4497e";
    hash = "sha256-w0WGF3+a/wt8CCNnYK2YaeJj4XugsS8mYqZEJk8MFe0=";
  };

  propagatedUserEnvPkgs = [calfw calfw-org];
  buildInputs = propagatedUserEnvPkgs;
}

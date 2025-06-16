{
  trivialBuild,
  fetchFromGitHub,
  calfw,
  calfw-org,
}:
trivialBuild rec {
  pname = "calfw-blocks";
  version = "unstable-2023-07-21";
  src = fetchFromGitHub {
    owner = "ml729";
    repo = "calfw-blocks";
    rev = "0fe829035ffa491c3f2610f05a7f1ec936a4497e";
    sha256 = "1v8m1i7jci56c8k2zcd0gghn7qk9k2nn0rr311y0pzwsgwbqcif3";
  };

  propagatedUserEnvPkgs = [calfw calfw-org];
  buildInputs = propagatedUserEnvPkgs;
}

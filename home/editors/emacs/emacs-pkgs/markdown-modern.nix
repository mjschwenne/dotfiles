{
  trivialBuild,
  fetchFromGitHub,
}:
trivialBuild rec {
  pname = "markdown-modern";
  version = "unstable-2026-10-13";
  src = fetchFromGitHub {
    owner = "rjprins";
    repo = "markdown-modern";
    rev = "58e1d0e6cdeaa6c8610bb90acdf3ba3083e22ae1";
    hash = "sha256-i9I1jGr3ixbcOyRqpblCC+ehCfuMKzeUmTwB0V/+Auk=";
  };

  # The elisp lives in lisp/, but trivialBuild byte-compiles and installs
  # `*.el` from the top of the source tree, so descend into it first.
  sourceRoot = "${src.name}/lisp";
}

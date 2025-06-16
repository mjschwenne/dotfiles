{
  trivialBuild,
  fetchFromGitHub,
}:
trivialBuild {
  pname = "org-modern-indent";
  version = "unstable-2025-04-12";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "org-modern-indent";
    rev = "9973bd3b91e4733a3edd1fca232208c837c05473";
    sha256 = "09bg1shw06ddlxq678abr9q6g9wfc6b483xi0iwldxk4qbgzgpdj";
  };
}

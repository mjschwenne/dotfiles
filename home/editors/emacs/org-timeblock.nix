{
  trivialBuild,
  fetchFromGitHub,
  org-ql, 
  persist,
}:

trivialBuild rec {
  pname = "org-timeblock";
  version = "main-25-08-2-2023";
  src = fetchFromGitHub {
    owner = "ichernyshovvv";
    repo = "org-timeblock";
    rev = "474c75d570fda74a947353a37cd860e2976ba93c";
    hash = "sha256-Nw/hwfw+NKyD8mNyKKTkpJccOUw4lD4OsPqfB02fRRU=";
  };

  propagatedUserEnvPkgs = [ org-ql persist ];
  buildInputs = propagatedUserEnvPkgs;
}

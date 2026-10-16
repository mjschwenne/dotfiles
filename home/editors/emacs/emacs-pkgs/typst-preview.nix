{
  trivialBuild,
  fetchFromGitHub,
  websocket,
}:
trivialBuild rec {
  pname = "typst-preview";
  version = "unstable-2026-10-15";
  src = fetchFromGitHub {
    owner = "havarddj";
    repo = "typst-preview.el";
    rev = "f2903a1b98e13be7c927de835ae0d9159dd9fb9a";
    hash = "sha256-1OpJAeLg54KHoDaCcUKb64HLUrdc2fUOhy59abGEoX0=";
  };

  propagatedUserEnvPkgs = [ websocket ];
  buildInputs = propagatedUserEnvPkgs;
}

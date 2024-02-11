{
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation rec {
  pname = "oneui4-icons";
  version = "2024-02-04";
  dontBuild = true;

  src = fetchFromGitHub {
    owner = "end-4";
    repo = "OneUI4-Icons";
    rev = "9ba21908f6e4a8f7c90fbbeb7c85f4975a4d4eb6";
    sha256 = "sha256-f5t7VGPmD+CjZyWmhTtuhQjV87hCkKSCBksJzFa1x1Y=";
    fetchSubmodules = true;
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons
    cp -r OneUI $out/share/icons
    cp -r OneUI-dark $out/share/icons
    cp -r OneUI-light $out/share/icons

    runHook postInstall
  '';
}

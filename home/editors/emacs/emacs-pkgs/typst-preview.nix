{
  trivialBuild,
  fetchFromGitHub,
  websocket,
}:
trivialBuild rec {
  pname = "typst-preview";
  version = "unstable-2025-10-01";
  src = fetchFromGitHub {
    owner = "havarddj";
    repo = "typst-preview.el";
    rev = "4f3decb8d9b85b1a06b0dfbd0fccaa0d2d2c24b4";
    hash = "sha256-rxXgIoILnxyn543keCkpuv80TLaSATN1T7hCvnM8inU=";
  };

  propagatedUserEnvPkgs = [websocket];
  buildInputs = propagatedUserEnvPkgs;
}

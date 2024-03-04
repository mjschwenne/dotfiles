{
  lib,
  buildPythonPackage,
  fetchPypi,
  # build-system
  poetry-core,
  # dependencies
  anyio,
  # tests
  pytest,
}:
buildPythonPackage rec {
  pname = "asyncer";
  version = "0.0.5";
  pyproject = true;

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-KXnz4Ey+3+XP63kCfc99AE/MRDCgygBmriBJDyGOwG4=";
  };

  build-system = [
    poetry-core
  ];

  dependencies = [
    anyio
  ];

  nativeCheckInputs = [
    pytest
  ];
}

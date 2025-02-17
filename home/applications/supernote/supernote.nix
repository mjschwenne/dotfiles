{
  fetchPypi,
  python3Packages,
}: let
  potracer = python3Packages.buildPythonPackage rec {
    pname = "potracer";
    version = "0.0.4";
    src = fetchPypi {
      inherit pname version;
      sha256 = "sha256-MsvbmERGBmvPvotgAUKlS5D6baJ0tpIZRzIF1uTAlxM=";
    };

    propagatedBuildInputs = with python3Packages; [
      numpy
    ];
  };
in
  python3Packages.buildPythonApplication rec {
    pname = "supernotelib";
    version = "0.6.2";
    format = "pyproject";
    src = fetchPypi {
      inherit version pname;
      sha256 = "sha256-90D79mCXKjOx5G/6j4v7C5Gsjid+AtPR62bjs/MgFVE=";
    };

    propagatedBuildInputs = with python3Packages; [
      hatchling
      colour
      fusepy
      numpy
      pillow
      pypng
      reportlab
      svglib
      svgwrite
      potracer
    ];
  }

{
  lib,
  pkgs,
  fetchPypi,
  ...
}: let
  orp = pkgs.callPackage ../obs/onnxruntime-python.nix {
    onnxruntime = pkgs.callPackage ../obs/onnxruntime.nix {cudaSupport = false;};
    inherit (pkgs.python3Packages) buildPythonPackage numpy packaging;
    inherit (pkgs.python3Packages) pythonRelaxDepsHook;
  };
in
  pkgs.python3Packages.buildPythonApplication rec {
    pname = "rembg";
    version = "2.0.55";
    pyproject = true;

    src = fetchPypi {
      inherit pname version;
      sha256 = "sha256-aR9pHBPi8tdAII6DuBQapfYb5bAhM34k1QKg8UpDo9U=";
    };

    dependencies = with pkgs.python3Packages; [
      jsonschema
      numpy
      orp
      opencv4
      pillow
      pooch
      pymatting
      scikit-image
      scipy
      tqdm
      aiohttp
      (callPackage ../../python-pkgs/asyncer.nix {
        inherit (pkgs.python3Packages) anyio pytest poetry-core;
      })
      click
      fastapi
      filetype
      gradio
      multipart
      uvicorn
      watchdog
    ];

    postPatch = ''
      sed -i 's/opencv-python-headless/opencv/' setup.py
    '';
  }

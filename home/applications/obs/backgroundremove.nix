{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchurl,
  fetchpatch,
  cmake,
  ninja,
  obs-studio,
  onnxruntime,
  cudatoolkit,
  cudaPackages,
  opencv,
  qt6,
  curl,
}:
stdenv.mkDerivation rec {
  pname = "obs-backgroundremoval";
  version = "0.5.19";

  src = fetchFromGitHub {
    owner = "occ-ai";
    repo = "obs-backgroundremoval";
    rev = "d58d4783a7bbc128ce90efbfdedbcfce3946b3ff";
    hash = "sha256-B8FvTq+ucidefIN3aqAJbezcHnTv6vYPxjYETiMiMFs=";
  };

  # patches = [
  #   # build without GPU support
  #   (fetchpatch {
  #     url = "https://patch-diff.githubusercontent.com/raw/occ-ai/obs-backgroundremoval/pull/524.patch";
  #     sha256 = "sha256-ewETfFcD/0zscsUAPO/1UpytHRdhq/eSrzDcRIUdAxY=";
  #   })
  #   # fix build without GPU support
  #   (fetchpatch {
  #     url = "https://patch-diff.githubusercontent.com/raw/occ-ai/obs-backgroundremoval/pull/530.patch";
  #     sha256 = "sha256-sPZ6Va3d2D0yHWN/bcrMBwNjCNqPXED6I7ipeSU+ThI=";
  #   })
  # ];

  nativeBuildInputs = [cmake ninja cudaPackages.cuda_nvcc cudatoolkit];
  buildInputs = [obs-studio onnxruntime opencv qt6.qtbase curl];

  dontWrapQtApps = true;

  cmakeFlags = [
    "-DUSE_SYSTEM_ONNXRUNTIME=ON"
    "-DUSE_SYSTEM_OPENCV=ON"
    "-DCUDA_TOOLKIT_ROOT_DIR=${cudatoolkit.lib}"
    # "-DDISABLE_ONNXRUNTIME_GPU=ON"
  ];

  postInstall = ''
    mkdir $out/lib $out/share
    mv $out/obs-plugins/64bit $out/lib/obs-plugins
    rm -rf $out/obs-plugins
    mv $out/data $out/share/obs
  '';

  meta = with lib; {
    description = "OBS plugin to replace the background in portrait images and video";
    homepage = "https://github.com/royshil/obs-backgroundremoval";
    maintainers = with maintainers; [zahrun];
    license = licenses.mit;
    platforms = ["x86_64-linux"];
  };
}

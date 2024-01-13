{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  obs-studio,
  onnxruntime,
  opencv,
  cudaPackages,
  cudatoolkit,
}:
stdenv.mkDerivation rec {
  pname = "obs-backgroundremoval";
  version = "0.5.19";

  src = fetchFromGitHub {
    owner = "royshil";
    repo = "obs-backgroundremoval";
    rev = "v${version}";
    hash = "sha256-BeV/rQKfmYvHfO+UmCDCjrx1RKUDE3y1ACFBBeOV8Ck=";
  };

  nativeBuildInputs = [cmake];
  buildInputs = [obs-studio onnxruntime opencv cudaPackages.cudnn cudaPackages.tensorrt cudatoolkit];

  dontWrapQtApps = true;

  cmakeFlags = [
    "-DUSE_SYSTEM_ONNXRUNTIME=ON"
    "-DUSE_SYSTEM_OPENCV=ON"
    "-DCUDA_TOOLKIT_ROOT_DIR=${cudatoolkit.lib}"
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
    platforms = ["x86_64-linux" "i686-linux"];
  };
}

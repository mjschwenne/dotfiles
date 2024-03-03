{
  pkgs,
  pkgs-stable,
  osConfig,
  ...
}: let
  opencv_no_cuda = pkgs.opencv.override {enableCuda = false;};
  backgroundremove = pkgs-stable.obs-studio-plugins.obs-backgroundremoval.override {
    opencv = opencv_no_cuda;
  };
in {
  # Build with cuda on terra only
  home.packages =
    if osConfig.networking.hostName == "terra"
    then [
      # pkgs.cudaPackages.tensorrt
      # pkgs.cudaPackages.cudnn
      (pkgs.callPackage
        ./onnxruntime.nix
        {})
    ]
    else [];

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins;
      [
        wlrobs
        obs-pipewire-audio-capture
      ]
      ++ [backgroundremove];
  };

  home.file.".config/obs-studio/themes" = {
    source = ./catppuccin;
    recursive = true;
  };
}

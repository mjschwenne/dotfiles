{
  pkgs,
  osConfig,
  ...
}: let
  backgroundremove = pkgs.callPackage ./backgroundremove.nix {};
in {
  # Build with cuda on terra only
  home.packages =
    if osConfig.networking.hostName == "terra"
    then [
      pkgs.cudaPackages.tensorrt
      pkgs.cudaPackages.cudnn
      pkgs.onnxruntime
    ]
    else [];

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-backgroundremoval
      obs-pipewire-audio-capture
    ];
    # ++ [backgroundremove];
  };

  home.file.".config/obs-studio/themes" = {
    source = ./catppuccin;
    recursive = true;
  };
}

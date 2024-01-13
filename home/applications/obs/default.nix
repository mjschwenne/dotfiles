{pkgs, ...}: let
  backgroundremove = pkgs.callPackage ./backgroundremove.nix {};
in {
  home.packages = [
    pkgs.cudaPackages.tensorrt
    pkgs.cudaPackages.cudnn
    pkgs.onnxruntime
  ];

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

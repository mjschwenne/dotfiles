{
  pkgs,
  pkgs-stable,
  osConfig,
  ...
}: {
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins;
      [
        wlrobs
        obs-pipewire-audio-capture
	(obs-backgroundremoval.override {onnxruntime = 
	  pkgs.onnxruntime.override {cudaSupport = false;};})
      ];
  };

  home.file.".config/obs-studio/themes" = {
    source = ./catppuccin;
    recursive = true;
  };
}

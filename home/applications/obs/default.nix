{ config, pkgs, ... }@inputs:

{
	# home.packages = with pkgs; [
	# 	obs-studio
	# ];

    programs.obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-backgroundremoval 
        obs-pipewire-audio-capture
      ];
    };

	home.file.".config/obs-studio/themes" = {
		source = ./catppuccin;
		recursive = true;
	};
}

{ config, pkgs, ... }@inputs:

{
	home.packages = with pkgs; [
		obs-studio
	];

	home.file.".config/obs-studio/themes" = {
		source = ./catppuccin;
		recursive = true;
	};
}

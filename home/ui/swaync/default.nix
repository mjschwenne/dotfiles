{ config, pkgs, ... }@inputs:

{
	home.packages = with pkgs; [
		swaynotificationcenter
		libnotify
	];

	home.file.".config/swaync/style.css" = {
		source = ./mocha.css;
	};
}

{ config, pkgs, ... }@inputs:

{
	home.file.".config/BetterDiscord/themes/mocha.theme.css" = {
		source = ./mocha.theme.css;
	};

	home.packages = with pkgs; [
		discord
		betterdiscordctl	
	];
}

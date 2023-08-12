{ config, pkgs, ... }@inputs:

{
	home.packages = with pkgs; [
		wlogout
	];

	home.file.".config/wlogout" = {
		source = ./wlogout;
		recursive = true;
	};
}

{ config, pkgs, ... }@inputs:

{
	imports = [
		./swaync
		./rofi
		./wlogout
		./swaylock
	];
}

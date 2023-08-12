{ config, pkgs, ... }@inputs:

{
	imports = [
		./discord 
		./spotify
		./obs
	];
}

{ pkgs, ... }:

{
	home.packages = with pkgs; [
      R 
      rstudio
	];
}

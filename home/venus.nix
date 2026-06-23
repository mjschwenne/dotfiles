{ pkgs, ... }:
{
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  imports = [
    ./common.nix
    ./graphical.nix
    ./applications
    ./applications/obs
  ];

  home.packages = with pkgs; [
    keymapp
    kdePackages.kdenlive
  ];
}

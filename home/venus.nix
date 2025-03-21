{pkgs, ...}: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  imports = [
    ./common.nix
    ./graphical.nix
    ./applications
  ];

  home.packages = with pkgs; [
    keymapp
  ];
}

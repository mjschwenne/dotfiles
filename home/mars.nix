{pkgs, ...}: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  imports = [
    ./common.nix
    ./graphical.nix
    ./applications
    ./applications/r
  ];

  home.packages = with pkgs; [
    keymapp
  ];
}

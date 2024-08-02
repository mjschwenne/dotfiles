{...}: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  imports = [
    ./common.nix
    ./graphical.nix
    ./applications
    ./applications/r
  ];
}

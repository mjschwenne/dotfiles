{...}: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  imports = [
    ./common.nix
    ./graphical.nix
    ./ui
    ./applications
    ./applications/wonderdraft
    ./applications/dungeondraft
    ./applications/r
    ./editors/emacs
  ];
}

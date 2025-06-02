{...}: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  imports = [./common.nix ./applications/media-gui];
}

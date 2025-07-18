{
  pkgs,
  spicetify-nix,
  ...
}: let
  spicePkgs = spicetify-nix.legacyPackages.${pkgs.stdenv.system};
in {
  imports = [spicetify-nix.homeManagerModules.default];

  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes.sleek;
    colorScheme = "Nord";
    windowManagerPatch = true;
  };

  xdg.desktopEntries = {
    spotify = {
      name = "Spotify";
      genericName = "Music Player";
      exec = "spotify %U";
      terminal = false;
      categories = ["Audio" "Music" "Player" "AudioVideo"];
      icon = "spotify-client";
    };
  };
}

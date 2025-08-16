{spicetify-nix, ...}: {
  imports = [spicetify-nix.homeManagerModules.default];

  programs.spicetify = {
    enable = true;
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

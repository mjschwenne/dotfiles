{
  pkgs,
  spicetify-nix,
  ...
}: let
  spicePkgs = spicetify-nix.packages.${pkgs.system}.default;
in {
  imports = [spicetify-nix.homeManagerModule];

  home.packages = with pkgs; [
    spotify-player
    ncspot
  ];

  programs.spicetify = {
    enable = true;
    colorScheme = "custom";
    # rose pine color scheme
    customColorScheme = {
      text = "ebbcba";
      subtext = "F0F0F0";
      sidebar-text = "e0def4";
      main = "191724";
      sidebar = "2a2837";
      player = "191724";
      card = "191724";
      shadow = "1f1d2e";
      selected-row = "797979";
      button = "31748f";
      button-active = "31748f";
      button-disabled = "555169";
      tab-active = "ebbcba";
      notification = "1db954";
      notification-error = "eb6f92";
      misc = "6e6a86";
    };
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

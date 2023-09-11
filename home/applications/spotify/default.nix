{ pkgs
, spicetify-nix
, ...
}:
let
  spicePkgs = spicetify-nix.packages.${pkgs.system}.default;
in
{
  imports = [ spicetify-nix.homeManagerModule ];

  home.packages = with pkgs; [ spotify-player ];

  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes.catppuccin-mocha;
    colorScheme = "peach";
    windowManagerPatch = true;
  };

  xdg.desktopEntries = {
    spotify = {
      name = "Spotify";
      genericName = "Music Player";
      exec = "spotify %U";
      terminal = false;
      categories = [ "Audio" "Music" "Player" "AudioVideo" ];
      icon = "spotify-client";
    };
  };
}

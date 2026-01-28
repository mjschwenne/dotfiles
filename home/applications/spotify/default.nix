{ spicetify-nix, pkgs, ... }:
{
  imports = [ spicetify-nix.homeManagerModules.default ];

  programs.spicetify =
    let
      spicePkgs = spicetify-nix.legacyPackages.${pkgs.stdenv.system};
    in
    {
      enable = true;
      windowManagerPatch = false;
      enabledExtensions = with spicePkgs.extensions; [
        songStats
        adblock
        aiBandBlocker
      ];
    };

  xdg.desktopEntries = {
    spotify = {
      name = "Spotify";
      genericName = "Music Player";
      exec = "spotify %U";
      terminal = false;
      categories = [
        "Audio"
        "Music"
        "Player"
        "AudioVideo"
      ];
      icon = "spotify-client";
    };
  };
}

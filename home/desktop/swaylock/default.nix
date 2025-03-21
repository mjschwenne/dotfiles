{pkgs, ...}: {
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock;
  };

  xdg.configFile = {
    "swaylock/config".source = ./config;
    "swaylock/swaylock.png".source = ./swaylock.png;
  };
}

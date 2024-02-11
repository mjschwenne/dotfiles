{pkgs, ...}: {
  home = {
    packages = with pkgs; [swaylock-effects swayidle];
  };

  xdg.configFile = {
    "swaylock/config".source = ./config;
    "swaylock/swaylock.png".source = ./swaylock.png;
  };
}

{pkgs, ...}: {
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
  };

  # home = {
  #   packages = with pkgs; [swaylock-effects swayidle];
  # };

  xdg.configFile = {
    "swaylock/config".source = ./config;
    "swaylock/swaylock.png".source = ./swaylock.png;
  };
}

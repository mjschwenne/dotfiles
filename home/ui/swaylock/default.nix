{pkgs, ...}: {
  home.packages = with pkgs; [swaylock-effects swayidle];

  # home.file.".config/swaylock/config" = { source = ./config; };
  #
  # home.file.".config/swaylock/swaylock.png" = { source = ./swaylock.png; };
}

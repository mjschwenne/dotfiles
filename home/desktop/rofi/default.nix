{
  config,
  pkgs,
  ...
} @ inputs: {
  home.file.".config/rofi/config.rasi" = {
    source = ./config.rasi;
  };

  home.file.".local/share/rofi/themes" = {
    source = ./themes;
    recursive = true;
  };

  home.packages = with pkgs; [
    rofi-wayland
  ];
}

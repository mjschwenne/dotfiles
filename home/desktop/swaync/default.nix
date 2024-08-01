{pkgs, ...}: {
  home.packages = with pkgs; [
    swaynotificationcenter
    libnotify
  ];

  xdg.configFile = {
    "swaync/style.css".source = ./style.css;
    "swaync/config.json".source = ./config.json;
  };
}

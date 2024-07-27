{pkgs, ...}: {
  home.packages = with pkgs; [
    swaynotificationcenter
    libnotify
  ];

  xdg.configFile = {
    # "swaync/style.css".source = ./mocha.css;
    "swaync/style.css".text = ''
      @import "notification.css";
      @import "center.css";
    '';
    "swaync/notification.css".source = ./notification.css;
    "swaync/center.css".source = ./center.css;
    "swaync/config.json".source = ./config.json;
  };
}

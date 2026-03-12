{ pkgs, ... }:
{
  services.mako = {
    enable = true;
    settings = {
      max-visible = 10;
      layer = "top";
      anchor = "top-right";
      icons = true;
      icon-path = "/etc/profiles/per-user/mjs/share/icons/Papirus/";
      actions = true;
      border-radius = 10;
      max-icon-size = 64;
      margin = 20;
      default-timeout = 3000;
    };
  };

  home.packages = with pkgs; [
    libnotify
  ];
}

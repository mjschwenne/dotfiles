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
      border-radius = 4;
      max-icon-size = 64;
      margin = 2;
      default-timeout = 3000;
      "mode=do-not-disturb" = {
        invisible = 1;
      };
    };
  };

  home.packages = with pkgs; [
    libnotify
  ];
}

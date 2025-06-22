{pkgs, ...}: {
  services.mako = {
    enable = true;
    settings = {
      max-visible = 10;
      layer = "top";
      anchor = "top-right";
      font = "JetBrainsNerdFont 12";
      icons = true;
      actions = true;
      background-color = "#4c566a";
      text-color = "#eceff4";
      border-color = "#5e81ac";
      border-radius = 10;
      max-icon-size = 64;
      margin = 20;
      default-timeout = 1000;
    };
  };

  home.packages = with pkgs; [
    libnotify
  ];
}

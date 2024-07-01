{pkgs, ...}: {
  xdg.configFile."vesktop/themes/rose-pine.theme.css".source = ./rose-pine.theme.css;

  home.packages = with pkgs; [
    vesktop
  ];
}

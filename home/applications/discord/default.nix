{pkgs, ...}: {
  xdg.configFile."vesktop/themes/mocha.theme.css".source = ./mocha.theme.css;

  home.packages = with pkgs; [
    vesktop
  ];
}

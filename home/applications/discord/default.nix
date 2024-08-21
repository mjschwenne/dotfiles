{pkgs, ...}: {
  xdg.configFile."vesktop/themes/mocha.theme.css".source = ./mocha.theme.css;
  xdg.configFile."vesktop/themes/nordic.vencord.css".source = ./nordic.vencord.css;

  home.packages = with pkgs; [
    vesktop
  ];
}

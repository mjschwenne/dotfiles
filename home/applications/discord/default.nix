{pkgs, ...}: {
  home.file.".config/VencordDesktop/VencordDesktop/themes/mocha.theme.css".source = ./mocha.theme.css;

  home.packages = with pkgs; [
    webcord
    vesktop
  ];
}

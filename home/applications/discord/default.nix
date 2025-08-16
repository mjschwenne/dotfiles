{pkgs, ...}: {
  home.packages = with pkgs; [
    vesktop
  ];

  stylix.targets.vesktop.enable = true;
}

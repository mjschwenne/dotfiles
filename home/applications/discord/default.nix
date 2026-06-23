{ pkgs, ... }:
{
  programs.vesktop.enable = true;

  home.packages = with pkgs; [ webcord ];

  stylix.targets.vesktop.enable = true;
}

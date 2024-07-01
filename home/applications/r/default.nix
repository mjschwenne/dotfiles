{pkgs, ...}: let
  r-with-packages = pkgs.rstudioWrapper.override {
    packages = with pkgs.rPackages; [ggplot2 quarto];
  };
in {home.packages = with pkgs; [quarto r-with-packages];}

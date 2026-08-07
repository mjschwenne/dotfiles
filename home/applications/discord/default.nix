{ pkgs, lib, ... }:
{
  programs.vesktop = {
    enable = true;
    package = pkgs.vesktop.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "Vencord";
        repo = "Vesktop";
        rev = "f11585586fa111bedff50cf632186b2d37d220ec";
        hash = "sha256-vvqdA4xi0AIidgCgJ8034kIrIDaRWMzoJMP/U4U133M=";
      };
    });
  };

  stylix.targets.vesktop.enable = true;
}

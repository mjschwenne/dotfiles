{
  osConfig,
  pkgs,
  ...
}: {
  home = {
    username = "mjs";
    homeDirectory = "/home/mjs";
  };

  imports = [
    ./common.nix
    ./graphical.nix
    ./ui
    ./applications
    ./applications/obs
    ./applications/wonderdraft
    ./applications/dungeondraft
    ./applications/r
    ./editors/emacs
  ];

  home.packages = with pkgs; [
    # Graphics
    (blender.override {
      cudaSupport = true;
    })
    krita

    # Audio
    audacity
    ardour
  ];
}

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
    ./applications
    ./applications/obs
    ./applications/wonderdraft
    ./applications/dungeondraft
    ./applications/r
  ];

  home.packages = with pkgs; [
    # Graphics
    # blender-hip
    krita

    keymapp

    # Audio
    audacity
    ardour

    # Networking
    qbittorrent
  ];
}

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
  ];

  home.packages = with pkgs; [
    # Graphics
    # blender-hip
    krita

    keymapp

    # Networking
    qbittorrent
  ];
}

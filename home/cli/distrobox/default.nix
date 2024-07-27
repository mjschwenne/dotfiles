{pkgs, ...}: {
  xdg.configFile."distrobox/distrobox.conf".text = ''
    container_additional_volumes="/nix/store:/nix/store:ro /etc/profiles/per-user:/etc/profiles/per-user:ro /etc/static/profiles/per-user:/etc/static/profiles/per-user:ro"
    container_image_default="registry.fedoraproject.org/fedora-toolbox:40"
  '';

  home.packages = with pkgs; [
    distrobox
  ];
}

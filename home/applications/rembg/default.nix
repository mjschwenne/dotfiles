{pkgs, ...}: {
  home.packages = with pkgs; [
    (callPackage ./rembg.nix {})
  ];
}

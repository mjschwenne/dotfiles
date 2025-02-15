{pkgs, ...}: {
  home.packages = [
    (pkgs.callPackage ./supernote.nix {})
  ];
}

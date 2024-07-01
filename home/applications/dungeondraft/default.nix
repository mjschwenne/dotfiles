{pkgs, ...}: {
  home.packages = [(pkgs.callPackage ./dungeondraft.nix {})];
}

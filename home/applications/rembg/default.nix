{pkgs, ...} @ inputs: {
  home.packages = with pkgs; [
    (callPackage ./rembg.nix {pkgs = inputs.pkgs-master;})
  ];
}

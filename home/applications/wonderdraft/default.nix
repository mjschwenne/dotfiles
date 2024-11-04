{pkgs, ...}: {
  # home.packages = with pkgs; [(pkgs.callPackage ./wonderdraft.nix {})];
  home.packages = with pkgs; [wonderdraft];

  # xdg.desktopEntries = {
  #   wonderdraft = {
  #     name = "Wounderdraft";
  #     exec = "wonderdraft %U";
  #     terminal = false;
  #     categories = ["Graphics"];
  #     type = "Application";
  #     icon = "wonderdraft";
  #   };
  # };
}

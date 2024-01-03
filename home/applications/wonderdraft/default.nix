{pkgs, ...}: {
  home.packages = with pkgs; [(pkgs.callPackage ./wonderdraft.nix {})];

  xdg.desktopEntries = {
    wonderdraft = {
      name = "Wounderdraft";
      exec = "wonderdraft %U";
      terminal = false;
      categories = ["Graphics"];
      type = "Application";
      icon = "wonderdraft";
    };
  };
}

{ pkgs, ... }: {
  home.packages = with pkgs; [ (pkgs.callPackage ./dungeondraft.nix { }) ];

  # xdg.desktopEntries = {
  #   dungeondraft = {
  #     name = "Dungeondraft";
  #     exec = "dungeondraft %U";
  #     terminal = false;
  #     categories = [ "Graphics" ];
  #     type = "Application";
  #     icon = "dungeondraft";
  #   };
  # };
}

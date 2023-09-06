{ pkgs, ... }: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  imports = [ ./common.nix ];

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [ tmux ];
}

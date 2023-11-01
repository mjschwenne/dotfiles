{ pkgs, ... }: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  # link the configuration file in current directory to the specified location in home directory
  # home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  # link all files in `./scripts` to `~/.config/i3/scripts`
  # home.file.".config/i3/scripts" = {
  #   source = ./scripts;
  #   recursive = true;   # link recursively
  #   executable = true;  # make all files executable
  # };

  # encode the file content in nix configuration file directly
  # home.file.".xxx".text = ''
  #     xxx
  # '';

  # basic configuration of git
  programs.git = {
    enable = true;
    userName = "Matt Schwennesen";
    userEmail = "mjschwenne@gmail.com";
  };

  programs.bash = {
    enable = true;
    shellAliases = { vi = "nvim"; };
  };

  # Some common user-level packages
  home.packages = with pkgs; [
    # Fetches
    neofetch
    pfetch

    # Utilities
    ripgrep
    eza
    ranger
    nix-prefetch-github # helps write derivations hosted on github
    speedtest-cli

    # Fun tools
    cowsay
    lolcat
    boxes
    figlet
    fortune
    tty-clock
  ];

  programs.nix-index.enable = true;

  imports = [ 
    # Neovim config
    ./editors/nvim
    
    # Fish config
    ./cli/fish.nix
  ];

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}

{
  pkgs,
  osConfig,
  ...
}: {
  imports =
    # Don't configure starship on sol
    if osConfig.networking.hostName == "sol"
    then [
      ./fish
    ]
    else [
      ./fish
      ./starship
    ];

  programs = {
    bash = {
      enable = true;
      shellAliases = {vi = "nvim";};
    };

    git = {
      enable = true;
      userName = "Matt Schwennesen";
      userEmail = "mjschwenne@gmail.com";
    };

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    nix-index.enable = true;
  };

  # Some common user-level packages
  home.packages = with pkgs; [
    # Fetches
    neofetch
    pfetch

    # Utilities
    ripgrep
    ripgrep-all
    eza
    ranger
    nix-prefetch-github # helps write derivations hosted on github
    speedtest-cli
    bc

    # Fun tools
    cowsay
    lolcat
    boxes
    figlet
    fortune
    tty-clock
  ];
}

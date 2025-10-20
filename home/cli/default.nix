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
      ./nushell
    ]
    else [
      ./fish
      ./nushell
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
      extraConfig = {
        diff.tool = "nvimdiff";
        merge.tool = "nvimdiff";
      };
    };

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    nix-index.enable = true;

    btop = {
      enable = true;
      settings = {
        theme_background = false;
      };
    };
  };

  # Some common user-level packages
  home.packages = with pkgs; [
    # Fetches
    fastfetch

    # Utilities
    ripgrep
    ripgrep-all
    eza
    nix-prefetch
    nix-prefetch-git
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

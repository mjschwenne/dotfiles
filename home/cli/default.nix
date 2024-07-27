{
  pkgs,
  osConfig,
  ...
}: {
  imports =
    # Don't configure starship or distrobox on sol
    if osConfig.networking.hostName == "sol"
    then [
      ./fish
    ]
    else [
      ./fish
      ./starship
      ./distrobox
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
    fastfetch

    # Utilities
    ripgrep
    ripgrep-all
    eza
    speedtest-cli
  ];
}

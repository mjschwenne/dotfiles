{
  lib,
  pkgs,
  nixpkgs,
  ...
} @ inputs: {
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nix = {
    channel.enable = true;
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = [
      "nixpkgs=${inputs.nixpkgs.outPath}"
    ];
    settings = {
      experimental-features = ["nix-command" "flakes"];
      trusted-users = ["root" "mjs"];
      trusted-public-keys = [
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
        "sol1:FnmeWYY2OGCZpx7+ZKBoOui6UrrUqASpap+FYHXMPsc="
      ];
      nix-path = "${inputs.nixpkgs.outPath}";
    };
  };

  nixpkgs.config = {
    permittedInsecurePackages = [
      "freeimage-unstable-2021-11-01"
    ];

    # Allow unfree packages
    allowUnfree = true;
  };

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
  hardware.uinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.mjs = {
    isNormalUser = true;
    description = "Matt Schwennesen";
    extraGroups = ["docker" "networkmanager" "wheel" "syncthing" "video" "input" "uinput"];
    shell = pkgs.fish;
  };

  sops = {
    defaultSopsFile = ../secrets.yaml;
    defaultSopsFormat = "yaml";

    age.keyFile = "/home/mjs/.config/sops/age/keys.txt";
  };

  # Catppuccin
  catppuccin = {
    flavor = "mocha";
    accent = "pink";
  };
  console.catppuccin.enable = true;

  # Need to enable the user shell program
  programs.fish.enable = true;

  # Power management
  services.upower.enable = true;

  environment.systemPackages = with pkgs; [
    # Languages
    # C
    gcc
    strace
    ltrace
    gdb

    # Python
    (python311.withPackages
      (ps: with ps; [debugpy]))

    # Rust
    rustc
    cargo

    # Java
    jdk17

    # Haskell
    ghc
    haskellPackages.ghci-dap
    cabal-install
    cabal2nix

    # Build Utilities
    gnumake
    cmake

    # Version control
    git

    # NixOS
    nix-output-monitor
    home-manager

    # CLI Utilities
    fzf
    tree
    which
    file
    gawk
    htop
    calc
    rename
    smartmontools
    killall

    # Networking
    wget
    curl
    dnsutils
    ldns
    nmap
    iperf3

    # Hardware Interfaces
    brightnessctl
    wirelesstools

    # Archives
    gnutar
    zip
    unzip
    zstd
    xz
    p7zip

    # Spellcheck
    enchant
    hunspell
    hunspellDicts.en_US-large

    # Security
    sops
    gnupg
    age
  ];

  # Text editor
  programs.neovim = {
    enable = true;
    defaultEditor = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

{pkgs, ...}: {
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.trusted-users = ["root" "mjs"];

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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.mjs = {
    isNormalUser = true;
    description = "Matt Schwennesen";
    extraGroups = ["networkmanager" "wheel" "syncthing"];
    shell = pkgs.fish;
  };

  # Need to enable the user shell program
  programs.fish.enable = true;

  # Power management
  services.upower.enable = true;

  # Some common packages for all of my devices
  # List packages installed in system profile. To search, run:
  # $ nix search wget
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

    # Julia
    julia-bin

    # Java
    jdk17

    # Haskell
    ghc
    haskellPackages.ghci-dap

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
    gnupg
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

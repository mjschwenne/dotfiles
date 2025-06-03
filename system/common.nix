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
    channel.enable = false;
    registry.nixpkgs.flake = inputs.nixpkgs;
    settings = {
      experimental-features = ["nix-command" "flakes"];
      trusted-users = ["root" "mjs"];
      trusted-public-keys = [
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
        "sol1:FnmeWYY2OGCZpx7+ZKBoOui6UrrUqASpap+FYHXMPsc="
      ];
      nix-path = "nixpkgs=flake:nixpkgs";
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
    extraGroups = ["docker" "networkmanager" "wheel" "syncthing" "video" "input" "uinput" "audio"];
    shell = pkgs.fish;
  };

  sops = {
    defaultSopsFile = ../file.yaml;
    defaultSopsFormat = "yaml";

    age.keyFile = "/home/mjs/.config/sops/age/keys.txt";
  };

  # Nord TTY
  console.colors = [
    "2E3440"
    "3B4252"
    "434C5E"
    "4C566A"
    "D8DEE9"
    "E5E9F0"
    "ECEFF4"
    "8FBCBB"
    "BF616A"
    "D08770"
    "EBCB8B"
    "A3BE8C"
    "88C0D0"
    "81A1C1"
    "B48EAD"
    "5E81AC"
  ];

  # Need to enable the user shell program
  programs.fish.enable = true;

  # Power management
  services.upower.enable = true;
  # Tailscale VPN
  services.tailscale = {
    enable = true;
    # For use with Mullvad VPN exit node
    useRoutingFeatures = "client";
    extraSetFlags = ["--exit-node=us-det-wg-002.mullvad.ts.net"];
  };

  systemd.services.mjs-tailscale-up = {
    enable = true;
    after = ["tailscaled.service" "sys-subsystem-net-devices-tailscale0.device"];
    wantedBy = ["multi-user.target"];
    serviceConfig.Type = "oneshot";
    script = ''
      timeout 60s ${pkgs.bash}/bin/bash -c "until ${pkgs.tailscale}/bin/tailscale status --peers=false; do sleep 1; done"
    '';
  };

  environment.systemPackages = with pkgs; [
    # Languages
    # C
    gcc
    strace
    ltrace
    gdb

    # Python
    (python313.withPackages
      (ps: with ps; [debugpy]))

    # Rust
    rustc
    cargo

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
    rename
    smartmontools
    killall

    # Networking
    wget
    curl
    dnsutils
    ldns
    nmap

    # Hardware Interfaces
    brightnessctl
    wirelesstools

    # Archives
    gnutar
    zip
    unzip
    zstd
    xz

    # Spellcheck
    enchant
    hunspell
    hunspellDicts.en_US-large
    (aspellWithDicts (dicts:
      with dicts; [
        en
        en-computers
        en-science
      ]))

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
}

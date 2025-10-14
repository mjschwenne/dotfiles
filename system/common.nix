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

    secrets = {
      "ai/mistral/key".owner = "mjs";
      "calendar/work".owner = "mjs";
      "calendar/taa".owner = "mjs";
      "calendar/taa-com".owner = "mjs";
    };
  };

  programs.nix-ld.enable = true;

  # Need to enable the user shell program
  programs.fish.enable = true;

  # Power management
  services.upower.enable = true;
  # Tailscale VPN
  services.tailscale = {
    enable = true;
    # For use with Mullvad VPN exit node
    useRoutingFeatures = "client";
    extraUpFlags = ["--exit-node=us-den-wg-203.mullvad.ts.net"];
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

    # Build Utilities
    gnumake

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
    man-pages

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

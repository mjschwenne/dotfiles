# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{ config
, pkgs
, lib
, ...
} @ inputs: {
  imports = [
    # Include the results of the hardware scan.
    ./luna-hardware.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "luna"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable =
    true; # Easiest to use and most distros use this by default.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.mjs = {
    isNormalUser = true;
    description = "Matt Schwennesen";
    extraGroups = [ "networkmanager" "wheel" "surface-control" ];
    shell = pkgs.fish;
  };

  security.pam.services.swaylock = { };

  microsoft-surface.surface-control.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Setup cache for hyprland so I don't have to compile it manually every time
  nix.settings = {
    extra-substituters = [ "https://hyprland.cachix.org" "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    trusted-users = [ "mjs" ];
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.sessionVariables.NIXOS_OZONE_WL = "1"; # enable wayland for electron apps
  environment.systemPackages = with pkgs; [
    # compilers
    gcc
    rustc
    rustfmt
    rust-analyzer
    cargo

    # version control
    git

    # nix related
    #
    # it provides the command `nom` works just like `nix`
    # with more details log output
    nix-output-monitor

    # utilities
    fzf # command line fuzzy finder
    tree # visualize file tree
    file # learn more about a file
    brightnessctl # control backlights at hardware level
    wirelesstools

    # archives
    gnutar
    zip
    unzip
    xz
    p7zip

    # system monitor
    htop

    # networking
    wget
    curl
    dnsutils
    ldns
    nmap
    iperf3

    # debug
    strace
    ltrace

    # SDDM theme
    libsForQt5.qt5.qtgraphicaleffects

    # Spellcheck
    enchant
    hunspell
    hunspellDicts.en_US-large

    # Build utils
    gnumake
    cmake

    # libraries
    pkg-config
    fontconfig
  ];

  # Setup SDDM display manager
  services.xserver.displayManager.sddm.sugarCandyNix = {
    enable = true;

    settings = {
      ScreenWidth = 1920;
      ScreenHeight = 1080;
      Background = lib.cleanSource ./sddm.png;
      PartialBlur = true;
      FormPosition = "left";
      Font = "JetBrainsMono Nerd Font";
      ForceHideCompletePassword = true;
      DateFormat = "dddd, dd MMMM yyyy";
    };
  };

  # Install hyprland wayland compositor
  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
  };

  # File manager
  services.gvfs.enable = true;
  services.tumbler.enable = true;
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [ thunar-archive-plugin thunar-volman ];
  };

  programs.fish.enable = true;

  # Text editor
  programs.neovim = {
    enable = true;
    defaultEditor = true;
  };

  programs.steam.enable = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Power management
  services.upower.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  services.syncthing = {
    enable = true;
    user = "mjs";
    dataDir = "/home/mjs/syncthing";

    settings = {
      devices = {
        "terra" = {
          id = "SA2KNKU-LTH27QL-OHZSGVW-HCMWWFP-5RYZEER-K3LQKDM-NGF2PNQ-HRVKSQL";
        };
        "sol" = {
          id = "73R7LD7-CE75DOS-QYCX3IN-7MYT33V-W65EKXH-L5Y2HIU-UGUPESG-KZEUYAF";
        };
      };

      folders = {
        "org" = {
          path = "/home/mjs/Documents";
          devices = [ "terra" "sol" ];
        };

        "kdb" = {
          path = "/home/mjs/kdb";
          devices = [ "terra" "sol" ];
        };
      };
    };
  };

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

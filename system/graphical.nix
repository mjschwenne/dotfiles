{pkgs, ...}: {
  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.sessionVariables.NIXOS_OZONE_WL = "1"; # enable wayland for electron apps
  environment.systemPackages = with pkgs; [
    # SDDM theme
    libsForQt5.qt5.qtgraphicaleffects

    # Install window manager and greeter
    swayfx
    greetd.tuigreet

    # Misc Utils
    libtool
  ];

  fonts.fontconfig.enable = true;
  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
  ];

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = ''${pkgs.greetd.tuigreet}/bin/tuigreet --remember --time --cmd "sway"'';
        user = "mjs";
      };
    };
  };
  services.gnome.gnome-keyring.enable = true;
  security.pam.services = {
    greetd = {
      enableGnomeKeyring = true;
      gnupg.enable = true;
    };
    login = {
      enableGnomeKeyring = true;
      gnupg.enable = true;
    };
    swaylock = {};
  };

  programs.dconf.enable = true;

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  # Need flatpak to use distrobox-host-exec
  services.flatpak.enable = true;

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
    config.common.default = ["wlr"];
  };

  # File manager
  services.gvfs.enable = true;
  services.tumbler.enable = true;
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [thunar-archive-plugin thunar-volman];
  };

  # Bluetooth
  hardware.enableRedistributableFirmware = true;
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.gnupg.agent = {
    enable = true;
    # enableSSHSupport = false;
    # pinentryPackage = pkgs.pinentry-gnome3;
  };

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
  };
}

{
  config,
  pkgs,
  lib,
  ...
} @ inputs: {
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

    # Audio Utilities
    # pulseaudio
  ];

  systemd.user.services.ydotoold = {
    enable = true;
    description = "An auto-input utility for wayland";
    serviceConfig.ExecStart = "/etc/profiles/per-user/mjs/bin/ydotoold --socket-path /tmp/ydotools";
    wantedBy = ["default.target"];
  };

  fonts.fontconfig.enable = true;
  fonts.packages = with pkgs; [
    (nerdfonts.override {fonts = ["JetBrainsMono" "SpaceMono"];})
    (google-fonts.override {
      fonts = [
        "Gabarito"
        "Lexend"
      ];
    })
    material-symbols
    rubik
  ];

  # Setup SDDM display manager
  security.pam.services.swaylock = {};
  services.displayManager.sddm = {
    wayland.enable = true;

    sugarCandyNix = {
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
  };

  services.xserver.displayManager.session = [
    {
      manage = "window";
      name = "SwayFX";
      start = ''
        env WLR_RENDERER=vulkan ${inputs.swayfx.packages."${pkgs.system}".swayfx-unwrapped}/bin/sway --unsupported-gpu
        waitPID=$!
      '';
    }
  ];

  # Install hyprland wayland compositor
  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    portalPackage = inputs.hyprland.packages."${pkgs.system}".xdg-desktop-portal-hyprland;
  };

  programs.dconf.enable = true;

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
    config.common.default = "*";
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
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = false;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
  };
}

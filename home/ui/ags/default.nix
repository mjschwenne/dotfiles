{pkgs, ...} @ inputs: let
  python_packages = with pkgs.python311Packages; [
    build
    material-color-utilities
    pillow
    poetry-core
    pywal
    setuptools-scm
    wheel
  ];
  gnome_packages = with pkgs.gnome; [
    # gnome-keyring
    gnome-control-center
    gnome-bluetooth
  ];
  wayland-idle-inhibitor = pkgs.stdenv.mkDerivation {
    name = "wayland-idle-inhibitor";
    propagatedBuildInputs = [
      (pkgs.python311.withPackages (pythonPackages:
        with pythonPackages; [
          pywayland
        ]))
    ];
    dontUnpack = true;
    installPhase = "install -Dm755 ${./wayland-idle-inhibitor.py} $out/bin/wayland-idle-inhibitor";
  };
in {
  imports = [inputs.ags.homeManagerModules.default];

  programs.ags = {
    enable = true;
    configDir = ./config;

    extraPackages = with pkgs; [
      gtksourceview
      gtksourceview4
      python311Packages.material-color-utilities
      pywal
      sassc
      webkitgtk
      webp-pixbuf-loader
      ydotool
    ];
  };

  # Install all packages from the dependencies.conf file.
  # Hyprland is excluded and is installed elsewhere
  home.packages = with pkgs;
    [
      # Basic
      coreutils
      cliphist
      curl
      fuzzel
      rsync
      wget
      ripgrep
      gojq
      nodePackages.npm
      typescript
      gjs
      sassc

      # Basic graphic
      xorg.xrandr

      # Player and Audio
      pavucontrol
      wireplumber
      libdbusmenu-gtk3
      playerctl
      swww

      # GTK deps
      webp-pixbuf-loader
      gtk-layer-shell
      gtk3
      gtksourceview
      gtksourceview4
      gobject-introspection
      # upower enabled via nixos services
      yad
      ydotool

      # Gnome
      polkit_gnome
      blueberry
      # networkmanager managed by nixos
      brightnessctl
      wlsunset

      # Widgets
      wayland-idle-inhibitor
      swayidle
      swaylock-effects
      wlogout
      wl-clipboard
      hyprpicker
      grim
      tesseract
      slurp

      # Fonts and Themes
      adw-gtk3
      gradience
      (callPackage ./oneui.nix {})

      # Applications
      foot
      starship
      # fish is managed elsewhere

      # Screenshot and recorder
      swappy
      wf-recorder
    ]
    ++ python_packages
    ++ gnome_packages;

  fonts.fontconfig.enable = true;
}

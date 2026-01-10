{
  pkgs,
  pkgs-master,
  pkgs-stable,
  awww,
  ...
}:
let
  packages = with pkgs; [
    # Web browsers
    firefox
    brave

    # Instant Communications
    slack
    zoom-us
    kbfs
    keybase
    keybase-gui
    signal-desktop

    # Network Utilities
    remmina
    openconnect
    trayscale

    # Password Manager
    keepassxc

    # Email
    thunderbird
    protonmail-bridge
    protonmail-desktop

    # Document-based Applications
    xournalpp
    poppler-utils
    pdfcpu
    pdfpc

    # Graphics Applications
    gimp
    inkscape

    # typesetting
    texliveFull
    ghostscript
    typst

    # Graphical Utilities
    blueberry
    pavucontrol
    playerctl
    networkmanagerapplet
    meld
    qalculate-gtk
    eog
    mate.engrampa
    swappy
    wl-mirror
    wdisplays

    # Games
    cockatrice
    melonDS

    # CLI Utilities
    pandoc
    hugo
    btop
    bluez
    inotify-tools
    ledger
    python313Packages.gpustat
    distrobox
    graphviz

    # Wayland Utilities
    awww.packages.${stdenv.hostPlatform.system}.default
    wev
    wl-clipboard
    wlr-randr
    kanshi
    wayland-logout
  ];
  masterPkgs = with pkgs-master; [
    protonvpn-gui
    zotero
  ];
  stablePkgs = with pkgs-stable; [ ];
in
{
  imports = [
    ./desktop
    ./applications
  ];

  stylix = {
    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 32;
    };
    icons = {
      enable = true;
      dark = "Papirus";
      light = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
    opacity = {
      terminal = 0.9;
      applications = 0.9;
    };
  };
  gtk.gtk3.extraConfig = {
    gtk-decoration-layout = "appmenu:none";
  };
  home.sessionVariables = {
    GNOME_KEYRING_CONTROL = "/run/user/1000/keyring";
    TERMINAL = "/etc/profiles/per-user/mjs/bin/wezterm";
    MOZ_ENABLE_DBUS = 1;
  };

  # Packages and fonts that should be installed to the user profile.
  fonts.fontconfig.enable = true;

  home.packages = packages ++ masterPkgs ++ stablePkgs;

  services = {
    gnome-keyring.enable = true;
    gpg-agent.pinentry.package = pkgs.pinentry-gnome3;
    keybase.enable = true;
    kbfs.enable = true;
  };

  xdg = {
    configFile = {
      "mimeapps.list".force = true;
      "distrobox/distrobox.conf".text = ''
        container_additional_volumes="/nix/store:/nix/store:ro /etc/profiles/per-user/mjs:/etc/profiles/per-user/mjs:ro /etc/static/profiles/per-user/mjs:/etc/static/profiles/per-user/mjs:ro /run/secrets:/run/secrets:ro"
      '';
      "Kvantum/Nord" = {
        source = ./desktop/qt/Nord;
        recursive = true;
      };
    };
    mimeApps = {
      enable = true;

      defaultApplications = {
        "text/html" = "librewolf.desktop";
        "x-scheme-handler/http" = "librewolf.desktop";
        "x-scheme-handler/https" = "librewolf.desktop";
        "x-scheme-handler/about" = "librewolf.desktop";
        "x-scheme-handler/unknown" = "librewolf.desktop";

        "text/calendar" = "thunderbird.desktop";
        "x-scheme-handler/mailto" = "thunderbird.desktop";

        "image/png" = "org.gnome.eog.desktop";
        "image/jpeg" = "org.gnome.eog.desktop";
        "image/jpg" = "org.gnome.eog.desktop";

        "application/pdf" = "org.pwmt.zathura.desktop";
      };
    };
  };
}

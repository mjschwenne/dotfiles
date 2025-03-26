{
  osConfig,
  pkgs,
  pkgs-master,
  pkgs-stable,
  ...
}: let
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
    protonmail-bridge-gui

    # Document-based Applications
    xournalpp
    poppler_utils
    nextcloud-client
    pdfcpu

    # Graphics Applications
    gimp
    inkscape

    # LaTeX
    (texlive.combine {
      inherit
        (texlive)
        scheme-medium
        wrapfig
        capt-of
        sfmath
        standalone
        preprint
        preview
        paralist
        nicematrix
        pgfplots
        cancel
        fontawesome
        tcolorbox
        environ
        tikzfill
        tikzmark
        enumitem
        dashrule
        ifmtarg
        multirow
        changepage
        biblatex
        lato
        fontaxes
        curve
        silence
        fontawesome5
        relsize
        comment
        csquotes
        xpatch
        cochineal
        xstring
        cabin
        inconsolata
        upquote
        fifo-stack
        varwidth
        tabto-ltx
        totcount
        ;
    })
    ghostscript
    biber

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

    # Programming languages
    coq

    # Wayland Utilities
    swww
    wev
    wl-clipboard
    wlr-randr
    kanshi
    grim
    slurp
    wayland-logout
  ];
  masterPkgs = with pkgs-master; [protonvpn-cli protonvpn-gui zotero];
  stablePkgs = with pkgs-stable; [];
in {
  imports = [./desktop ./applications];

  programs.ssh = {
    matchBlocks = {
      "sol" = {
        user = "mjs";
        hostname = "192.168.0.206";
        identitiesOnly = true;
        identityFile = osConfig.sops.secrets."ssh/${osConfig.networking.hostName}/sol/key".path;
      };
    };
  };

  gtk = {
    enable = true;
    theme = {
      name = "Nordic";
      package = pkgs.nordic;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-nord;
    };
    font = {name = "JetBrainsMono Nerd Font";};
    gtk3.extraConfig = {gtk-decoration-layout = "appmenu:none";};
  };
  home.pointerCursor = {
    name = "Nordzy-cursors";
    gtk.enable = true;
    package = pkgs.nordzy-cursor-theme;
  };
  home.sessionVariables = {
    GTK_THEME = "Nordic";
    GNOME_KEYRING_CONTROL = "/run/user/1000/keyring";
    MOZ_ENABLE_DBUS = 1;
  };

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style = {
      name = "kvantum";
      package = pkgs.libsForQt5.qtstyleplugin-kvantum;
    };
  };
  # Packages and fonts that should be installed to the user profile.
  fonts.fontconfig.enable = true;

  home.packages = packages ++ masterPkgs ++ stablePkgs;

  services = {
    gnome-keyring.enable = true;
    gpg-agent.pinentryPackage = pkgs.pinentry-gnome3;
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

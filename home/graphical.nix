{
  osConfig,
  pkgs,
  pkgs-master,
  ...
}: let
  packages = with pkgs; [
    # Web browsers
    brave

    # Instant Communications
    slack
    zoom-us

    # Network Utilities
    remmina
    openconnect

    # Password Manager
    keepassxc

    # Email
    thunderbird
    pass-wayland
    pass-secret-service
    protonmail-bridge
    protonmail-desktop
    protonmail-bridge-gui

    # Document-based Applications
    evince
    xournalpp
    rnote
    poppler_utils
    nextcloud-client

    # Graphics Applications
    gimp
    inkscape
    zotero_7
    mpv
    posterazor
    imagemagick

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
        tikzfill
        tikzmark
        enumitem
        biblatex
        curve
        silence
        csquotes
        mathtools
        ;
    })
    ghostscript

    # Graphical Utilities
    blueberry
    pavucontrol
    playerctl
    networkmanagerapplet
    meld
    qalculate-gtk
    webcamoid
    gnome.eog
    mate.engrampa
    swappy
    wl-mirror

    # Games
    cockatrice
    melonDS
    # mindustry-wayland

    # CLI Utilities
    pandoc
    hugo
    btop
    bluez
    socat
    jq
    jp
    jc
    inotify-tools
    google-cloud-sdk
    ledger
    python311Packages.gpustat

    # Programming languages
    coq

    # Wayland Utilities
    swww

    wev
    wl-clipboard
    wlsunset
    wlr-randr
    kanshi
    grim
    slurp
    waypaper
    wayland-logout
  ];
  masterPkgs = with pkgs-master; [librewolf firefox protonvpn-cli protonvpn-gui];
in {
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
      name = "rose-pine";
      package = pkgs.rose-pine-gtk-theme;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override {
        flavor = "mocha";
        accent = "pink";
      };
    };
    font = {name = "JetBrainsMono Nerd Font";};
    gtk3.extraConfig = {gtk-decoration-layout = "appmenu:none";};
  };
  home.sessionVariables = {
    GTK_THEME = "rose-pine";
  };
  home.pointerCursor = {
    package = pkgs.rose-pine-cursor;
    name = "BreezeX-RosePine-Linux";
  };

  qt = {
    enable = true;
    platformTheme.name = "qtct";
    style.name = "kvantum";
    style.package = pkgs.libsForQt5.qtstyleplugin-kvantum;
  };
  home.file.".config/Kvantum/Catppuccin-Mocha-Pink" = {
    source = ./ui/qt/Catppuccin-Mocha-Pink;
    recursive = true;
  };
  imports = [./ui ./applications ./editors/emacs];

  # Packages and fonts that should be installed to the user profile.
  fonts.fontconfig.enable = true;

  home.packages = packages ++ masterPkgs;

  xdg = {
    configFile = {
      "mimeapps.list".force = true;
      "Kvantum/kvantum.kvconfig".source = (pkgs.formats.ini {}).generate "kvantum.kvconfig" {
        General.theme = "Catppuccin-Mocha-Pink";
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

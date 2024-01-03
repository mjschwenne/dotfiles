{
  pkgs,
  pkgs-master,
  lib,
  ...
}: let
  packages = with pkgs; [
    # Web browsers
    # librewolf
    # firefox
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

    # Document-based Applications
    evince
    xournalpp
    libsForQt5.okular
    poppler_utils

    # Graphics Applications
    gimp
    inkscape
    zotero

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

    # MTG
    cockatrice

    # CLI Utilities
    pandoc
    hugo
    glow
    btop
    iotop
    iftop
    bluez
    socat
    jq
    jp
    jc
    inotify-tools
    google-cloud-sdk
    ledger

    # Programming languages
    coq

    # Wayland Utilities
    eww-wayland
    swww

    wev
    wl-clipboard
    wlsunset
    wlr-randr
    kanshi
    grim
    slurp
    hyprpicker
    waypaper
  ];
  masterPkgs = with pkgs-master; [librewolf firefox];
in {
  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Mocha-Standard-Pink-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = ["pink"];
        variant = "mocha";
      };
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
    GTK_THEME = "Catppuccin-Mocha-Standard-Pink-Dark";
  };
  home.pointerCursor = {
    package = pkgs.catppuccin-cursors.mochaLight;
    name = "Catppuccin-Mocha-Light-Cursors";
  };

  qt = {
    enable = true;
    platformTheme = "qtct";
    style.name = "kvantum";
    style.package = pkgs.libsForQt5.qtstyleplugin-kvantum;
  };
  home.file.".config/Kvantum/Catppuccin-Mocha-Pink" = {
    source = ./ui/qt/Catppuccin-Mocha-Pink;
    recursive = true;
  };
  xdg.configFile."Kvantum/kvantum.kvconfig".source = (pkgs.formats.ini {}).generate "kvantum.kvconfig" {
    General.theme = "Catppuccin-Mocha-Pink";
  };

  imports = [./ui ./applications ./editors/emacs];

  # Packages and fonts that should be installed to the user profile.
  fonts.fontconfig.enable = true;

  home.packages = packages ++ masterPkgs;

  programs.kitty = {
    enable = true;
    theme = "Catppuccin-Mocha";
    font.name = "JetBrainsMono Nerd Font";
    shellIntegration.mode = "no-cursor";
    settings = {
      disable_ligatures = "never";
      cursor_shape = "block";
      share_connections = "no";
    };
  };

  # starship - an customizable prompt for any shell
  programs.starship = {enable = true;};

  home.file.".config/starship.toml" = {
    source = ./applications/starship.toml;
  };

  xdg.configFile."mimeapps.list".force = true;
  xdg.mimeApps = {
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
}

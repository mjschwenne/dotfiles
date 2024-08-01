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
    inotify-tools
    google-cloud-sdk
    ledger
    python311Packages.gpustat
    distrobox

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

  catppuccin = {
    flavor = "mocha";
    accent = "pink";
    pointerCursor = {
      enable = true;
      accent = "dark";
    };
  };

  gtk = {
    enable = true;
    catppuccin = {
      enable = true;
      icon.enable = true;
    };
    font = {name = "JetBrainsMono Nerd Font";};
    gtk3.extraConfig = {gtk-decoration-layout = "appmenu:none";};
  };
  home.sessionVariables = {
    # GTK_THEME = "catppuccin-mocha-pink-standard+default";
    GNOME_KEYRING_CONTROL = "/run/user/1000/keyring";
  };

  qt = {
    enable = true;
    platformTheme.name = "qtct";
    style.catppuccin = {
      enable = true;
      apply = true;
    };
  };
  imports = [./desktop ./applications ./editors/emacs];

  # Packages and fonts that should be installed to the user profile.
  fonts.fontconfig.enable = true;

  home.packages = packages ++ masterPkgs;

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

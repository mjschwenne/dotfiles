{ pkgs, ... }: {
  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Mocha-Standard-Pink-dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "pink" ];
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
    font = { name = "JetBrainsMono Nerd Font"; };
    # cursorTheme = {
    #   name = "Catppuccin-Mocha-Light-Cursors";
    #   package = pkgs.catppuccin-cursors.mochaLight;
    # };
    gtk3.extraConfig = { gtk-decoration-layout = "appmenu:none"; };
  };
  home.pointerCursor = {
    package = pkgs.catppuccin-cursors.mochaLight;
    name = "Catppuccin-Mocha-Light-Cursors";
  };

  imports = [ ./ui ./applications ./editors/emacs ];

  # Packages and fonts that should be installed to the user profile.
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    # Web browsers
    librewolf 
    firefox
    brave 

    # Instant Communications
    slack
    zoom-us

    # Password Manager
    keepassxc

    # Email 
    thunderbird
    pass-wayland
    pass-secret-service
    protonmail-bridge

    # Misc Graphical Applications
    evince
    gimp
    xournalpp

    # LaTeX
    (texlive.combine {
      inherit (texlive) scheme-medium wrapfig capt-of sfmath;
    })

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
    globalprotect-openconnect

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
  ];

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
  programs.starship = { enable = true; };

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

      "image/png" = "org.gnome.eog.desktop";
      "image/jpeg" = "org.gnome.eog.desktop";
      "image/jpg" = "org.gnome.eog.desktop";
    };
  };
}
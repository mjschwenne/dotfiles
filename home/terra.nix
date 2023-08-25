{ config
, pkgs
, ...
} @ inputs: {
  home.username = "mjs";
  home.homeDirectory = "/home/mjs";

  nixpkgs.overlays = [
    # (import ./ui/mako/mako.nix)
    inputs.emacs-overlay.overlay
  ];

  # link the configuration file in current directory to the specified location in home directory home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  # link all files in `./scripts` to `~/.config/i3/scripts`
  # home.file.".config/i3/scripts" = {
  #   source = ./scripts;
  #   recursive = true;   # link recursively
  #   executable = true;  # make all files executable
  # };

  # encode the file content in nix configuration file directly
  # home.file.".xxx".text = ''
  #     xxx
  # '';

  # basic configuration of git, please change to your own
  programs.git = {
    enable = true;
    userName = "Matt Schwennesen";
    userEmail = "mjschwenne@gmail.com";
  };

  home.file.".config/eww" = {
    source = ./ui/eww;
    recursive = true;
  };

  home.file.".config/zathura/zathurarc".text = ''
       set default-fg                "#CDD6F4"
    set default-bg 			          "#1E1E2E"

    set completion-bg		          "#313244"
    set completion-fg		          "#CDD6F4"
    set completion-highlight-bg	  "#575268"
    set completion-highlight-fg	  "#CDD6F4"
    set completion-group-bg		    "#313244"
    set completion-group-fg		    "#89B4FA"

    set statusbar-fg		          "#CDD6F4"
    set statusbar-bg		          "#313244"

    set notification-bg		        "#313244"
    set notification-fg		        "#CDD6F4"
    set notification-error-bg	    "#313244"
    set notification-error-fg	    "#F38BA8"
    set notification-warning-bg	  "#313244"
    set notification-warning-fg	  "#FAE3B0"

    set inputbar-fg			          "#CDD6F4"
    set inputbar-bg 		          "#313244"

    set recolor-lightcolor		    "#1E1E2E"
    set recolor-darkcolor		      "#CDD6F4"

    set index-fg			            "#CDD6F4"
    set index-bg			            "#1E1E2E"
    set index-active-fg		        "#CDD6F4"
    set index-active-bg		        "#313244"

    set render-loading-bg		      "#1E1E2E"
    set render-loading-fg		      "#CDD6F4"

    set highlight-color		        "#575268"
    set highlight-fg              "#F5C2E7"
    set highlight-active-color	  "#F5C2E7"
    set recolor
  '';

  home.file.".config/hypr/scripts" = {
    source = ./ui/scripts;
    recursive = true;
  };

  home.file.".config/hypr/hyprland.conf".text = ''
     	# colors (catppuccin mocha)
    $rosewaterAlpha = f5e0dc
    $flamingoAlpha  = f2cdcd
    $pinkAlpha      = f5c2e7
    $mauveAlpha     = cba6f7
    $redAlpha       = f38ba8
    $maroonAlpha    = eba0ac
    $peachAlpha     = fab387
    $yellowAlpha    = f9e2af
    $greenAlpha     = a6e3a1
    $tealAlpha      = 94e2d5
    $skyAlpha       = 89dceb
    $sapphireAlpha  = 74c7ec
    $blueAlpha      = 89b4fa
    $lavenderAlpha  = b4befe

    $textAlpha      = cdd6f4
    $subtext1Alpha  = bac2de
    $subtext0Alpha  = a6adc8

    $overlay2Alpha  = 9399b2
    $overlay1Alpha  = 7f849c
    $overlay0Alpha  = 6c7086

    $surface2Alpha  = 585b70
    $surface1Alpha  = 45475a
    $surface0Alpha  = 313244

    $baseAlpha      = 1e1e2e
    $mantleAlpha    = 181825
    $crustAlpha     = 11111b

    $rosewater = 0xfff5e0dc
    $flamingo  = 0xfff2cdcd
    $pink      = 0xfff5c2e7
    $mauve     = 0xffcba6f7
    $red       = 0xfff38ba8
    $maroon    = 0xffeba0ac
    $peach     = 0xfffab387
    $yellow    = 0xfff9e2af
    $green     = 0xffa6e3a1
    $teal      = 0xff94e2d5
    $sky       = 0xff89dceb
    $sapphire  = 0xff74c7ec
    $blue      = 0xff89b4fa
    $lavender  = 0xffb4befe

    $text      = 0xffcdd6f4
    $subtext1  = 0xffbac2de
    $subtext0  = 0xffa6adc8

    $overlay2  = 0xff9399b2
    $overlay1  = 0xff7f849c
    $overlay0  = 0xff6c7086

    $surface2  = 0xff585b70
    $surface1  = 0xff45475a
    $surface0  = 0xff313244

    $base      = 0xff1e1e2e
    $mantle    = 0xff181825
    $crust     = 0xff11111b

    # environment variables
    env = GTK_THEME,Catppuccin-Mocha-Standard-Pink-dark
    env = BROWSER,librewolf

    # autostarts
    exec-once = dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
    exec-once = ~/.config/hypr/scripts/autostart.fish &
    exec-once = swayidle -w timeout 300 'swaylock' timeout 600 'systemctl suspend' before-sleep 'swaylock'

    # monitor rules
    monitor = ,preferred,auto,1
       monitor = eDP-1,1920x1080,0x0,1

    general {
    	cursor_inactive_timeout = 3;
    	border_size = 3
    	gaps_in = 5
    	gaps_out = 10
    	col.active_border = $green
    	col.inactive_border = $peach
    	col.group_border = $yellow
    	col.group_border_active = $green
    	col.group_border_locked = $red
    	col.group_border_locked_active = $pink
    	resize_on_border = true;
    }

    decoration {
    	rounding = 5
    }

    input {
    	kb_options = ctrl:nocaps
    	numlock_by_default = true
    	scroll_method = 2fg
    	float_switch_override_focus = 2
    }

    misc {
    	force_hypr_chan = true
    	enable_swallow = true
    	swallow_regex = ".*kitty.*"
    }

    dwindle {
    	preserve_split = true
    }

    # WINDOW RULES
    windowrule=float,qalculate-gtk
    windowrule=pin,rofi
    windowrule=workspace 8,thunderbird
    windowrule=float,title:^(Write:)(.*)(- Thunderbird)$
    windowrule=workspace 9,Spotify
    windowrule=workspace 10,discord
    windowrule=float,title:^(Open Files)$

    # ANIMATIONS
    bezier=overshot,0.05,0.9,0.1,1.1
    animation=windows,1,5,default,slide

    # KEYBINDINGS
       $mod = SUPER

    # Application hotkeys
    bind = $mod, F1, exec, thunar
    bind = $mod, F2, exec, keepassxc
    bind = $mod, F3, exec, librewolf
    bind = $mod, F4, exec, brave
    bind = $mod, F5, exec, firefox
    bind = $mod, F6, exec, qalculate-gtk
    bind = $mod, F7, exec, emacs
    bind = $mod, F8, exec, thunderbird
    bind = $mod, F9, exec, spotify
    bind = $mod, F10, exec, discord
    bind = $mod, F11, exec, evince
    bind = $mod, F12, exec, pavucontrol
    bind = $mod, RETURN, exec, kitty
    bind = $mod, b, exec, librewolf
    bind = $mod_SHIFT, b, exec, brave
    bind = $mod, r, exec, rofi -show drun
    bind = $mod, t, exec, swaync-client -t

    # system controls
    bind = $mod, z, exec, ~/.config/hypr/scripts/lock.fish
    bind = $mod_CTRL, z, exec, wlogout -b 5 -T 400 -B 400
    bind = $mod_CTRL, escape, exec, hyprctl kill
    bind = $mod_CTRL, r, exec, hyprctl reload
    bind = $mod_CTRL, e, exec, eww reload
    bind = $mod, e, exec, ~/.config/hypr/scripts/eject.fish

    # utility keys
    bind = ,Print, exec, grim -g "$(slurp)"
    bind = SHIFT, Print, exec, grim
    bind = , XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_SINK@ 0.05+
    bind = , XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_SINK@ 0.05-
    bind = , XF86AudioMute, exec, wpctl set-mute @DEFAULT_SINK@ toggle
    bind = , XF86AudioPlay, exec, playerctl play-pause
    bind = , XF86AudioNext, exec, playerctl next
    bind = , XF86AudioPrev, exec, playerctl previous
    bind = , XF86AudioStep, exec, playerctl stop
    bind = , XF86MonBrightnessUp, exec, brightnessctl s 5%+
    bind = , XF86MonBrightnessDown, exec, brightnessctl s 5%-

    # window managment
    bind = $mod, u, fullscreen, 0
    bind = $mod_SHIFT, u, fullscreen, 1
    bind = $mod, i, togglefloating
    bind = $mod, p, pin
    bind = $mod, Left, movewindow, l
    bind = $mod, Right, movewindow, r
    bind = $mod, Up, movewindow, u
    bind = $mod, Down, movewindow, d
    bind = $mod, q, killactive

    bind = $mod_ALT, g, togglegroup
    bind = $mod_ALT_SHIFT, g, moveoutofgroup
    bind = $mod_ALT, n, changegroupactive, f
    bind = $mod_ALT_SHIFT, n, changegroupactive, b
    bind = $mod_ALT, l, lockactivegroup
    bind = $mod_ALT_SHIFT, l, lockgroups
    bind = $mod_ALT, h, moveintogroup, r
    bind = $mod_ALT, j, moveintogroup, d
    bind = $mod_ALT, k, moveintogroup, u
    bind = $mod_ALT, l, moveintogroup, l

       # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
       ${
      builtins.concatStringsSep "\n" (builtins.genList (x: let
        ws = let c = (x + 1) / 10; in builtins.toString (x + 1 - (c * 10));
      in ''
        bind = $mod, ${ws}, exec, ~/.config/hypr/scripts/try_swap_workspace ${
          toString (x + 1)
        }
        bind = $mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}
      '') 10)
    }

    bind = $mod, h, movefocus, l
    bind = $mod, j, movefocus, d
    bind = $mod, k, movefocus, u
    bind = $mod, l, movefocus, r

    bind = $mod_SHIFT, h, swapwindow, l
    bind = $mod_SHIFT, j, swapwindow, d
    bind = $mod_SHIFT, k, swapwindow, u
    bind = $mod_SHIFT, l, swapwindow, r

    bind = $mod_CTRL, h, resizeactive, -10 0
    bind = $mod_CTRL, j, resizeactive, 0 10
    bind = $mod_CTRL, k, resizeactive, 0 -10
    bind = $mod_CTRL, l, resizeactive, 10 0

    bind = $mod, w, layoutmsg, preselect u
    bind = $mod, a, layoutmsg, preselect l
    bind = $mod, s, layoutmsg, preselect d
    bind = $mod, d, layoutmsg, preselect r

    bind = $mod, n, cyclenext
    bind = $mod_SHIFT, n, cyclenext, prev
    bind = $mod_CTRL, n, swapnext
    bind = $mod_CTRL_SHIFT, n, swapnext, prev

    bindm = $mod, mouse:272, movewindow
    bindm = $mod, mouse:273, resizewindow
  '';

  imports = [ ./ui ./applications ./editors/emacs ./editors/neovim.nix ];

  # Packages and fonts that should be installed to the user profile.
  fonts.fontconfig.enable = true;
  home.packages = with inputs.pkgs; [
    neofetch
    pfetch

    # browers
    librewolf
    firefox
    brave
    keepassxc
    zoom-us
    webcamoid
    pass-wayland
    pass-secret-service
    protonmail-bridge
    gimp
    wev
    slack
    globalprotect-openconnect

    # utils
    ripgrep # recursively searches directories for a regex pattern
    exa # A modern replacement for ‘ls’
    nix-prefetch-github # helps write derivations hosted on github
    blueberry # bluetooth manager
    pavucontrol # audio controller
    socat # bidirectional data transfer between sockets
    jq # JSON parser
    jp
    jc
    inotify-tools
    bluez # bluetooth cli
    playerctl # cli to control media players
    wl-clipboard # cli utility for copy pasta
    wlsunset # day/night color adjustments
    networkmanagerapplet
    pulseaudio

    # misc
    cowsay

    # productivity
    hugo # static site generator
    glow # markdown previewer in terminal

    btop # replacement of htop/nmon
    iotop # io monitoring
    iftop # network monitoring

    # Fonts
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })

    # Screenshots
    grim
    slurp

    # Hyprland helpers
    eww-wayland
    swww

    qalculate-gtk
    zathura
    evince
    thunderbird
    pandoc
    wlr-randr
    kanshi
    gnome.eog
  ];

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

  programs.fish = {
    enable = true;
    shellAliases = {
      vi = "nvim";
      dup-files = "find . -type f -printf '%p -> %f\\n' | sort -k2 | uniq -f1 --all-repeated=separate";
      flatten = ''
        find */ -type f -exec sh -c 'file=''${1#./}; mv "$file" "$(basename $file)"' _ '{}' \; ; find */ -depth -type d -exec rmdir '{}' \;'';
      mjs_bulk_rename = ''find . -depth -exec fish -c 'mjs_rename "{}"' \;'';
      icat = "kitty +kitten icat";
      ssh = "kitty +kitten ssh";
      m = "math";
      nix-shell = "nix-shell --run fish";
    };
    functions = {
      mjs_rename = ''
        for src in $argv
            set -l filename (string split -r -m 1 -f 1 '.' (basename $src))
            set -l extension (string split -r -m 1 -f 2 '.' (basename $src))
            # Remove '(', ')', '_', ' ' and "'" characters from the file name
            # Replace all '.' characters with '-'
            # Replace camelCase with kebab case
            # Remove extra consecutive '-' characters
            # Remove leading '-'
            # Remove leading '-' from subsequent parts of the file path
            # Lowercase any remaining capital letters
            # Replace '&' with 'and'
            set -l dest (echo $filename | sed -E \
                -e "s/[()_']//g" \
                -e "s/ [a-z]/\-\l&/g" \
                -e "s/ //g" \
                -e "s/[\.,]/\-/g" \
                -e "s/([a-z])([A-Z]+)/\1\-\L\2/g" \
                -e "s/\-\-+/\-/g" \
                -e "s/^\-//g" \
                -e "y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/" \
                -e "s/&/and/g"
            )

            if test -n $dest
                set dest (string join ''' (dirname $src) '/' $dest)
                if test -n $extension
                    set dest (string join '.' $dest $extension)
                end

                if test $src != $dest
                    mv -n $src $dest 2>&1 > /dev/null
                end
            end
        end
      '';
      fish_greeting = "pfetch";
    };
  };

  programs.bash = {
    enable = true;
    shellAliases = { vi = "nvim"; };
  };

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
    cursorTheme = {
      name = "Catppuccin-Mocha-Light-Cursors";
      package = pkgs.catppuccin-cursors.mochaLight;
    };
    gtk3.extraConfig = { gtk-decoration-layout = "appmenu:none"; };
    gtk4.extraConfig = { gtk-decoration-layout = "appmenu:none"; };
  };
  home.pointerCursor = {
    package = pkgs.catppuccin-cursors.mochaLight;
    name = "Catppuccin-Mocha-Light-Cursors";
  };

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}

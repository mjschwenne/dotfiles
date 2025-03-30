{
  pkgs,
  lib,
  osConfig,
  ...
} @ inputs: {
  home.packages = with pkgs; [
    # XMonad/Qtile switch workspace switching
    i3-wk-switch

    # overlay image on terminal
    swayimg

    # autotiling
    autotiling-rs

    # swallow windows, if prefixed by a command
    i3-swallow
  ];

  wayland.windowManager.sway = {
    enable = true;
    package = null;
    checkConfig = false;
    config = rec {
      modifier = "Mod4";
      terminal = "kitty";
      input = {
        "type:keyboard" = {
          "xkb_options" = "ctrl:nocaps";
        };
        "1386:968:Wacom_Intuos_BT_M_Pen" = {
          "map_to_output" =
            if osConfig.networking.hostName == "terra"
            then "DP-4"
            else "eDP-1";
        };
        # Luna touchscreen and stylus
        "1118:31:IPTSD_Virtual_Stylus_045E:001F" = {
          "map_to_output" = "eDP-1";
        };
        "1118:31:IPTSD_Virtual_Touchscreen_045E:001F" = {
          "map_to_output" = "eDP-1";
        };
      };
      keybindings = let
        mod = modifier;
        switch = "exec --no-startup-id ${pkgs.i3-wk-switch}/bin/i3-wk-switch";
      in
        lib.mkOptionDefault {
          # Application hotkeys
          # "${mod}+Return" = "exec ${inputs.ghostty.packages.${pkgs.system}.default}/bin/ghostty";
          "${mod}+Return" = "exec ${pkgs.kitty}/bin/kitty";
          "${mod}+b" = "exec ${pkgs.librewolf}/bin/librewolf";
          "${mod}+Shift+b" = "exec ${pkgs.firefox}/bin/firefox";
          "${mod}+e" = "exec emacsclient -c";
          "${mod}+Shift+e" = "exec emacs";
          "${mod}+p" = "exec ${pkgs.keepassxc}/bin/keepassxc";
          "${mod}+r" = "exec ${pkgs.rofi-wayland}/bin/rofi -show drun";

          "${mod}+F1" = "exec thunar";
          "${mod}+F2" = "exec ${pkgs.keepassxc}/bin/keepassxc";
          "${mod}+F3" = "exec ${pkgs.librewolf}/bin/librewolf";
          "${mod}+F4" = "exec ${pkgs.brave}/bin/brave";
          "${mod}+F5" = "exec ${pkgs.firefox}/bin/firefox";
          "${mod}+F6" = "exec ${pkgs.qalculate-gtk}/bin/qalculate-gtk";
          "${mod}+F7" = "exec emacsclient -c"; # Emacs is installed from the overlay, not pkgs
          "${mod}+F8" = "exec ${pkgs.thunderbird}/bin/thunderbird";
          "${mod}+F9" = "exec spotify"; # Spotify is also installed via overlay
          "${mod}+F10" = "exec ${pkgs.vesktop}/bin/vesktop";
          "${mod}+F11" = "exec ${pkgs.evince}/bin/evince";
          "${mod}+F12" = "exec ${pkgs.pavucontrol}/bin/pavucontrol";

          # System controls
          "${mod}+n" = "exec swaync-client -t";
          "${mod}+Control+q" = "exit";
          "${mod}+Control+r" = "reload";
          "${mod}+Control+v" = "exec pavucontrol";
          "${mod}+z" = "exec /home/mjs/.config/sway/scripts/lock.fish";
          "${mod}+Control+z" = "exec wlogout -b 5 -T 400 -B 400";

          # Window management
          "${mod}+q" = "kill";
          "${mod}+u" = "fullscreen";
          "${mod}+i" = "floating toggle";
          "${mod}+o" = "sticky";
          "${mod}+m" = "mode resize";
          "${mod}+d" = "layout default";
          "${mod}+w" = "layout toggle tabbed split";
          "${mod}+s" = "layout toggle stacking split";
          "${mod}+c" = "splith";

          # Workspace management
          "${mod}+1" = "${switch} 1";
          "${mod}+2" = "${switch} 2";
          "${mod}+3" = "${switch} 3";
          "${mod}+4" = "${switch} 4";
          "${mod}+5" = "${switch} 5";
          "${mod}+6" = "${switch} 6";
          "${mod}+7" = "${switch} 7";
          "${mod}+8" = "${switch} 8";
          "${mod}+9" = "${switch} 9";
          "${mod}+0" = "${switch} 10";

          # Utility keys
          "XF86AudioPlay" = "exec playerctl play-pause";
          "XF86AudioNext" = "exec playerctl next";
          "XF86AudioPrev" = "exec playerctl previous";
          "XF86AudioStop" = "exec playerctl stop";

          "XF86AudioRaiseVolume" = "exec swayosd-client --output-volume raise";
          "XF86AudioLowerVolume" = "exec swayosd-client --output-volume lower";
          "XF86AudioMute" = "exec swayosd-client --output-volume mute-toggle";
          "XF86AudioMicMute" = "exec swayosd-client --input-volume mute-toggle";

          "XF86MonBrightnessUp" = "exec swayosd-client --brightness raise";
          "XF86MonBrightnessDown" = "exec swayosd-client --brightness lower";

          "Print" = ''exec grim -g "$(slurp)" - | swappy -f -'';
          "Shift+Print" = ''exec grim -g "$(slurp)" - | wl-copy'';

          "Control+Alt+v" = "pkill fuzzel || cliphist list | fuzzel --no-fuzzy --dmenu | cliphist decode | wl-copy";
        };
      bars = [];
      colors = let
        base = "#2e3440";
        dark = "#2e3440";
        red = "#bf616a";
        orange = "#d08770";
        yellow = "#ebcb8b";
        green = "#a3be8c";
        blue = "#5e81ac";
        text = "#d8dee9";
      in {
        background = dark;
        focused = {
          background = green;
          border = green;
          childBorder = green;
          indicator = blue;
          text = base;
        };
        focusedInactive = {
          background = orange;
          border = orange;
          childBorder = orange;
          indicator = blue;
          text = base;
        };
        placeholder = {
          background = dark;
          border = base;
          childBorder = base;
          indicator = blue;
          text = text;
        };
        unfocused = {
          background = yellow;
          border = yellow;
          childBorder = yellow;
          indicator = blue;
          text = base;
        };
        urgent = {
          background = red;
          border = red;
          childBorder = red;
          indicator = blue;
          text = base;
        };
      };
      gaps = {
        right = 5;
        bottom = 5;
        left = 5;
        inner = 5;
        smartGaps = true;
        smartBorders = "no_gaps";
      };
      output =
        {
          "mars" = {
            DP-1 = {
              scale = "2";
              position = "0 0";
            };
            eDP-1 = {
              position = "1920 0";
            };
          };
          "venus" = {
            DP-1 = {
              scale = "1.6";
              position = "0 0";
            };
            eDP-1 = {
              position = "2400 0";
            };
          };
        }
        ."${osConfig.networking.hostName}"
        or {};
      assigns = {
        "8" = [{app_id = "^(thunderbird)$";}];
        "9" = [{title = "^(Spotify Premium)$";}]; # I swear spotify changes this with every update
        "10" = [{app_id = "^(vesktop)";}];
      };
      floating = {
        criteria = [
          {title = "^(Open Files?)(.*)$";}
          {title = "^(Select a File)(.*)$";}
          {title = "^(Choose wallpaper)(.*)$";}
          {title = "^(Open Folder)(.*)$";}
          {title = "^(Save As)(.*)$";}
          {title = "^(Library)(.*)$";}
          {app_id = "^(thunderbird)$";}
          {title = "^(Compact folders)$";}
          {title = "^(KeePassXC - Browser Access Request)$";}
          {title = "^(Unlock Database - KeePassXC)$";}
          {title = "^(Formula \\(pdflatex\\))$";}
          {app_id = "zenity";}
        ];
        modifier = modifier;
        titlebar = false;
      };
      focus = {
        followMouse = "yes";
        mouseWarping = "container";
      };
      window = {
        border = 3;
        # These are window rules
        commands = [
          # Inhibit idle
          {
            command = "inhibit_idle fullscreen";
            criteria = {app_id = "^.*";};
          }
          # Floats + size adjustments
          {
            command = "floating enable, resize set 400 200";
            criteria = {app_id = "qalculate-gtk";};
          }
          {
            command = "floating enable, resize set 350 100";
            criteria = {
              title = "^(Progress)$";
              app_id = "Zotero";
            };
          }
          # Make just the main thunderbird window tiling
          {
            command = "floating disable";
            criteria = {
              title = "^(.* - Mozilla Thunderbird)";
              app_id = "thunderbird";
            };
          }
        ];
        titlebar = false;
      };
      seat = {
        "*" = {
          hide_cursor = "3000";
        };
      };
      startup = [
        {command = "${pkgs.autotiling-rs}/bin/autotiling-rs";}
        {command = "eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)";}
        {command = "swww-daemon";}
        {command = "wl-paste --type text --watch cliphist store";}
        {command = "wl-paste --type image --watch cliphist store";}
        {command = "nm-applet";}
        {command = "/home/mjs/.config/sway/scripts/wallpaper.fish interval 300 &";}
        {command = "waybar";}
        {command = "protonmail-bridge --noninteractive &";}
        {command = "keepassxc";}
        {command = "systemctl --user import-environment DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP";}
        {command = "hash dbus-update-activation-environment 2>/dev/null && dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP XDG_CURRENT_DESKTOP=sway";}
        {command = "trayscale --hide-window";}
        {command = "nextcloud --background";}
      ];
    };
    extraConfig = ''
      corner_radius 10
      blur enable
      blur_passes 3
      layer_effects "waybar" blur enable
    '';
    systemd.enable = true;
  };

  services.swayosd = {
    enable = true;
  };

  xdg.configFile."sway/scripts" = {
    source = ./scripts;
    recursive = true;
  };
}

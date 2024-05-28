{
  pkgs,
  hyprland,
  hyprgrass,
  hyprland-contrib,
  osConfig,
  ...
}: {
  imports = [hyprland.homeManagerModules.default];

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [
      # hyprgrass.packages.${pkgs.system}.default
    ];
    # Hyprland changed the device format.. HM is still behind
    extraConfig = ''
      device {
        name = wacom-intuos-bt-m-pen
        transform = 0
        output = HDMI-A-5
      }
    '';
    settings = {
      animations = {
        enabled = true;
        bezier = [
          "linear, 0, 0, 1, 1"
          "md3_standard, 0.2, 0, 0, 1"
          "md3_decel, 0.05, 0.7, 0.1, 1"
          "md3_accel, 0.3, 0, 0.8, 0.15"
          "overshot, 0.05, 0.9, 0.1, 1.1"
          "crazyshot, 0.1, 1.5, 0.76, 0.92"
          "hyprnostretch, 0.05, 0.9, 0.1, 1.0"
          "fluent_decel, 0.1, 1, 0, 1"
          "easeInOutCirc, 0.85, 0, 0.15, 1"
          "easeOutCirc, 0, 0.55, 0.45, 1"
          "easeOutExpo, 0.16, 1, 0.3, 1"
        ];
        animation = [
          "windows, 1, 3, md3_decel, popin 60%"
          "border, 1, 10, default"
          "fade, 1, 2.5, md3_decel"
          "workspaces, 1, 7, fluent_decel, slide"
        ];
      };
      "$mod" = "SUPER";
      bind =
        [
          # Applications Hotkeys
          "$mod, F1, exec, thunar"
          "$mod, F2, exec, keepassxc"
          "$mod, F3, exec, librewolf"
          "$mod, F4, exec, brave"
          "$mod, F5, exec, firefox"
          "$mod, F6, exec, qalculate-gtk"
          "$mod, F7, exec, emacs"
          "$mod, F8, exec, thunderbird"
          "$mod, F9, exec, spotify"
          "$mod, F10, exec, vesktop"
          "$mod, F11, exec, evince"
          "$mod, F12, exec, pavucontrol"

          "$mod, RETURN, exec, wezterm"
          "$mod, b, exec, librewolf"
          "$mod_SHIFT, b, exec, firefox"
          "$mod, e, exec, emacs"
          "$mod, p, exec, keepassxc"
          "$mod, r, exec, rofi -show drun"

          # System
          "$mod, n, exec, swaync-client -t"
          "$mod_CTRL, q, exec, hyprctl kill"
          "$mod_CTRL, r, exec, hyprctl reload"
          ''$mod_CTRL, s, exec, XDG_CURRENT_DESKTOP="gnome" gnome-control-center''
          "$mod_CTRL, v, exec, pavucontrol"
          "$mod_CTRL, w, exec, waypaper --backend swww"
          "$mod, z, exec, ~/.config/hypr/scripts/lock.fish"
          "$mod_CTRL, z, exec, wlogout -b 5 -T 400 -B 400"

          # Window
          "$mod, q, killactive,"
          "$mod, u, fullscreen, 0"
          "$mod, i, togglefloating"
          "$mod, o, pin"

          # Window Movement
          "$mod_SHIFT, h, swapwindow, l"
          "$mod_SHIFT, j, swapwindow, d"
          "$mod_SHIFT, k, swapwindow, u"
          "$mod_SHIFT, l, swapwindow, r"
          "$mod_SHIFT, left, movewindow, l"
          "$mod_SHIFT, right, movewindow, r"
          "$mod_SHIFT, up, movewindow, u"
          "$mod_SHIFT, down, movewindow, d"

          # Window Focus
          "$mod, h, movefocus, l"
          "$mod, j, movefocus, d"
          "$mod, k, movefocus, u"
          "$mod, l, movefocus, r"
          "$mod, left, movefocus, l"
          "$mod, right, movefocus, r"
          "$mod, up, movefocus, u"
          "$mod, down, movefocus, d"
          "$mod, BracketLeft, movefocus, l"
          "$mod, BracketRight, movefocus, r"
          "ALT, Tab, cyclenext"
          "ALT, Tab, bringactivetotop,"
          "ALT_SHIFT, Tab, cyclenext, prev"

          # Window size
          "$mod_CTRL, h, resizeactive, -10 0"
          "$mod_CTRL, j, resizeactive, 0 10"
          "$mod_CTRL, k, resizeactive, 0 -10"
          "$mod_CTRL, l, resizeactive, 10 0"
          "$mod_CTRL, left, resizeactive, -10 0"
          "$mod_CTRL, down, resizeactive, 0 10"
          "$mod_CTRL, up, resizeactive, 0 -10"
          "$mod_CTRL, right, resizeactive, 10 0"

          # Window split direction
          "$mod, w, layoutmsg, preselect u"
          "$mod, a, layoutmsg, preselect l"
          "$mod, s, layoutmsg, preselect d"
          "$mod, d, layoutmsg, preselect r"

          # Workspaces
          "$mod, mouse_up, workspace, +1"
          "$mod, mouse_down, workspace, -1"
          "$mod_CTRL, mouse_up, workspace, +1"
          "$mod_CTRL, mouse_down, workspace, -1"
          "$mod_CTRL, BracketLeft, workspace, -1"
          "$mod_CTRL, BracketRight, workspace, +1"
          "$mod, Page_Down, workspace, +1"
          "$mod, Page_Up, workspace, -1"
          "$mod_CTRL, Page_Down, workspace, +1"
          "$mod_CTRL, Page_Up, workspace, -1"
          "$mod_ALT, Page_Down, movetoworkspace, +1"
          "$mod_ALT, Page_Up, movetoworkspace, -1"
          "$mod_SHIFT, Page_Down, movetoworkspace, +1"
          "$mod_SHIFT, Page_Up, movetoworkspace, -1"
          "$mod_CTRL_SHIFT, Right, movetoworkspace, +1"
          "$mod_CTRL_SHIFT, Left, movetoworkspace, -1"
          "$mod_SHIFT, mouse_down, movetoworkspace, -1"
          "$mod_SHIFT, mouse_up, movetoworkspace, +1"
          "$mod_ALT, mouse_down, movetoworkspace, -1"
          "$mod_ALT, mouse_up, movetoworkspace, +1"

          # Utility keys
          '',Print, exec, grim -g "$(slurp)" - | swappy -f -''
          ''ALT,Print,exec,grim -g "$(slurp)" - | wl-copy''
          "SHIFT,Print,exec,grim - | wl-copy"
          ''CTRL_$mod_SHIFT,t,exec,grim -g "$(slurp)" "tmp.png" && tesseract "tmp.png" - | wl-copy && rm "tmp.png"''
          "$mod_ALT, r, exec, ~/.config/ags/scripts/record-script.sh"
          "$mod_SHIFT, r, exec, ~/.config/ags/scripts/record-script.sh --fullscreen"
          "$mod_SHIFT_ALT, r, exec, ~/.config/ags/scripts/record-script.sh --fullscreen-sound"
          "$mod_ALT, c, exec, hyprpicker -a"
          "CTRL_ALT, v, exec, pkill fuzzel || cliphist list | fuzzel --no-fuzzy --dmenu | cliphist decode | wl-copy"

          # Touch gestures
          ",edge:d:u, exec, ags -t 'osk'"

          # Debug
          ''Super+Alt, f12, exec, notify-send 'Test notification' "Here's a really long message to test truncation and wrapping\nYou can middle click or flick this notification to dismiss it!" -a 'Shell' -A "Test1=I got it!" -A "Test2=Another action"''
          ''Super+Alt, Equal, exec, notify-send "Urgent notification" "Ah hell no" -u critical -a 'Hyprland keybind''
        ]
        ++
        # Workspaces
        (builtins.genList (x: let
            ws = let c = (x + 1) / 10; in builtins.toString (x + 1 - (c * 10));
          in ''
            $mod, ${ws}, exec, try_swap_workspace ${
              toString (x + 1)
            }
            bind=$mod_SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}
            bind=$mod_ALT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}
          '')
          10);
      binde = [
        # Window splits
        "Super, Minus, splitratio, -0.1"
        "Super, Equal, splitratio, 0.1"
        "Super, Semicolon, splitratio, -0.1"
        "Super, Apostrophe, splitratio, 0.1"
      ];
      bindl = [
        ", XF86AudioMute, exec, swayosd-client --output-volume mute-toogle"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPrev, exec, playerctl previous"
        ", XF86AudioStop, exec, playerctl stop"
      ];
      bindle = [
        ", XF86AudioRaiseVolume, exec, swayosd-client --output-volume raise"
        ", XF86AudioLowerVolume, exec, swayosd-client --output-volume lower"
        ", XF86MonBrightnessUp, exec, swayosd-client --brightness raise"
        ", XF86MonBrightnessDown, exec, swayosd-client --brightness lower"
      ];
      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];
      decoration = {
        rounding = 10;
        blur = {
          enabled = true;
          xray = false;
          size = 5;
          passes = 3;
          brightness = 1;
          noise = 0.01;
          contrast = 1;
        };
        drop_shadow = false;
        shadow_ignore_window = true;
        shadow_range = 20;
        shadow_offset = "0 2";
        shadow_render_power = 2;
        "col.shadow" = "rgba(0000001A)";
      };
      debug = {
        disable_logs = false;
      };
      dwindle = {
        preserve_split = true;
        no_gaps_when_only = 1;
      };
      exec-once = [
        "swww kill; swww-daemon"
        "dbus-update-activation-environment --all &"
        "sleep 1 && dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
        "wl-paste --type text --watch cliphist store"
        "wl-paste --type image --watch cliphist store"
        "protonmail-bridge --noniteractive &"
        "nm-applet"
        "swaync"
        "/home/mjs/.config/hypr/scripts/wallpaper.fish interval 300 &"
        "waybar"
        # I don't know why, but a phamton monitor has been appearing since I setup sway.
        # Trying to switch to it or pull workspaces from it hangs hyprland, so let's
        # nip that in the bud by disabling it a soon as possible.
        "hyprctl keyword monitor Unknown-1,disable"
      ];
      input = {
        float_switch_override_focus = 2;
        kb_layout = "us";
        kb_options = "ctrl:nocaps";
        numlock_by_default = true;
        repeat_delay = 500;
        repeat_rate = 35;
        scroll_method = "2fg";
        touchpad = {
          natural_scroll = false;
          disable_while_typing = true;
        };
      };
      general = {
        cursor_inactive_timeout = 3;
        "col.active_border" = "$green";
        "col.inactive_border" = "$peach";
        border_size = 3;
        gaps_in = 5;
        gaps_out = 5;
        layout = "dwindle";
        no_focus_fallback = true;
        resize_on_border = true;
      };
      group = {
        "col.border_active" = "$green";
        "col.border_inactive" = "$yellow";
        "col.border_locked_inactive" = "$red";
        "col.border_locked_active" = "$pink";
        groupbar = {
          font_size = 12;
          gradients = false;
          "col.active" = "$green";
          "col.inactive" = "$yellow";
          "col.locked_active" = "$red";
          "col.locked_inactive" = "$pink";
        };
      };
      layerrule = [
        "blur,waybar"
        "blur, swaylock"
      ];
      misc = {
        animate_manual_resizes = false;
        animate_mouse_windowdragging = false;
        enable_swallow = true;
        focus_on_activate = true;
        swallow_regex = "^foot$";
        vrr = 1;
      };
      monitor =
        [
          ",preferred,auto,1"
        ]
        ++ {
          "luna" = ["eDP-1,2736x1824,0x0,2"];
          "terra" = ["eDP-1,1920x1080,0x0,1"];
        }
        ."${osConfig.networking.hostName}"
        or [];
      plugin = {
        touch_gestures = {
          sensitivity = 4.0;
        };
      };
      source = [
        "~/.config/hypr/colors.conf"
      ];
      windowrule = [
        # General rules, like inhibit idle
        "idleinhibit fullscreen,(.+)"
        "pin, ^(showmethekey-gtk)$"

        # Dialogs
        "float,title:^(Open File)(.*)$"
        "float,title:^(Select a File)(.*)$"
        "float,title:^(Choose wallpaper)(.*)$"
        "float,title:^(Open Folder)(.*)$"
        "float,title:^(Save As)(.*)$"
        "float,title:^(Library)(.*)$ "

        # Floats
        "float,qalculate-gtk"
        "size 400 200,qalculate-gtk"
        "pin,rofi"
        "float,title:^(Write:)(.*)(- Thunderbird)$"
        "float,title:^(Write: \\(no subject\\))$"
        "float,title:^(Open Files)$"
        "float,title:^(Compact folders)$"
        "float,title:^(KeePassXC - Browser Access Request)$"
        "float,title:^(Unlock Database - KeePassXC)$"
        "float,title:^(Formula \\(pdflatex\\))$"

        # Workspaces
        "workspace 8,thunderbird"
        "workspace 9,title:^(Spotify Premium)$"
        "workspace 10,WebCord"
        "workspace 10,vesktop"
      ];
      windowrulev2 = [
        "float,class:^(zenity)$"
        "float,title:^(Progress),class:^(Zotero)$"
        "size 350 100,title:^(Progress),class:^(Zotero)$"
        "noblur,title:^(Annotation - Zoom)$"
      ];
    };
  };

  home = {
    sessionVariables = {
      BROWSER = "librewolf";
    };
    packages = with pkgs; [
      hyprshade
      fcitx5
      (callPackage ./hyprland-activewindow.nix {})
      (hyprland-contrib.packages.${pkgs.system}.try_swap_workspace.overrideAttrs (o: {
        patches =
          (o.patches or [])
          ++ [
            ./try_swap_workspace.patch
          ];
      }))
    ];
    file.".config/hypr/colors.conf".source = ./colors.conf;
  };

  xdg.configFile = {
    "hypr/scripts" = {
      source = ./scripts;
      recursive = true;
    };

    "hypr/shaders" = {
      source = ./shaders;
      recursive = true;
    };
  };
}

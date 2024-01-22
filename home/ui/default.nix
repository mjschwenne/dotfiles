{osConfig, ...}: {
  imports = [./swaync ./rofi ./wlogout ./swaylock ./eww];

  home.file.".config/hypr/scripts" = {
    source = ./scripts;
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

    # Debug
    debug:disable_logs = false

    # environment variables
    env = BROWSER,librewolf

    # autostarts
    exec-once = dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
    exec-once = ~/.config/hypr/scripts/autostart.fish &
    exec-once = swayidle -w timeout 300 'swaylock' timeout 600 'systemctl suspend' before-sleep 'swaylock'

    # monitor rules
    monitor = ,preferred,auto,1
    monitor = ${
      if osConfig.networking.hostName == "luna"
      then "eDP-1,2736x1824,0x0,2"
      else "eDP-1,1920x1080,0x0,1"
    }

    general {
    	cursor_inactive_timeout = 3;
    	border_size = 3
    	gaps_in = 5
    	gaps_out = 10
    	col.active_border = $green
    	col.inactive_border = $peach
    	resize_on_border = true;
    }

    group {
        col.border_active = $green
        col.border_inactive = $yellow
        col.border_locked_inactive = $red
        col.border_locked_active = $pink
        groupbar {
            font_size = 12
            gradients = false
            col.active = $green
            col.inactive = $yellow
            col.locked_active = $red
            col.locked_inactive = $pink
        }
    }

    decoration {
    	rounding = 5
    }
    blurls = bar

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
    windowrule=size 400 200,qalculate-gtk
    windowrule=pin,rofi
    windowrule=workspace 8,thunderbird
    windowrule=float,title:^(Write:)(.*)(- Thunderbird)$
    windowrule=float,title:^(Write: \(no subject\))$
    windowrule=workspace 9,title:^(Spotify Premium)$
    windowrule=workspace 10,WebCord
    windowrule=float,title:^(Open Files)$
    windowrule=float,class:^(zenity)$
    windowrule=float,title:^(Compact folders)$
    windowrule=float,title:^(KeePassXC - Browser Access Request)$
    windowrule=float,title:^(Unlock Database - KeePassXC)$
    windowrule=float,title:^(Formula \(pdflatex\))$
    windowrule=noblur,title:^(Annotation - Zoom)$
    windowrulev2=float,title:^(Progress),class:^(Zotero)$

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
    bind = $mod, F10, exec, webcord
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
    bind = $mod_CTRL, w, exec, waypaper --backend swww
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
    bind = $mod, o, fakefullscreen
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
        '')
        10)
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
}

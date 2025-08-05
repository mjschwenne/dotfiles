{
  pkgs,
  lib,
  osConfig,
  niri,
  waybar,
  swww,
  ...
}: {
  home.packages = with pkgs; [
    niri.packages."${system}".default
    glib
    xwayland-satellite
  ];

  programs.niriswitcher = {
    enable = true;
    settings = {
      keys = {
        modifier = "Super";
      };
      center_on_focus = true;
    };
    style =
      # css
      ''
        :root {
          --bg-color: rgba(67, 76, 94, 1);
          --label-color: rgb(216, 222, 233);
          --alternate-label-color: rgb(236, 239, 244);
          --dim-label-color: rgb(236, 239, 244);
          --border-color: rgba(46, 52, 64, 0.95);
          --highlight-color: rgba(76, 86, 106, 0.95);
          --urgency-color: rgb(191, 97, 106);
          --indicator-focus-color: rgba(94, 129, 172, 0.95);
          --indicator-color: rgba(76, 86, 106, 0.95);
        }
      '';
  };

  xdg.configFile."niri/scripts" = {
    source = ./scripts;
    recursive = true;
  };

  xdg.configFile."niri/config.kdl".text = let
    outputs =
      {
        "terra" =
          # kdl
          ''
            output "DP-3" {
                mode "3840x2160"
                scale 1.6
                transform "normal"
                position x=0 y=0
                background-color "#2e3440"
                backdrop-color "#434c5e"
                focus-at-startup
            }

            output "DP-4" {
                mode "3840x2160"
                scale 1.6
                transform "normal"
                position x=2400 y=0
                background-color "#2e3440"
                backdrop-color "#434c5e"
            }
          '';
        "venus" =
          # kdl
          ''
            output "DP-1" {
                mode "3840x2160"
                scale 1.6
                transform "normal"
                position x=0 y=0
                background-color "#2e3440"
                backdrop-color "#434c5e"
                focus-at-startup
            }

            output "eDP-1" {
                mode "1920x1200"
                scale 1
                transform "normal"
                position x=2400 y=0
                background-color "#2e3440"
                backdrop-color "#434c5e"
            }
          '';
        "mars" =
          # kdl
          ''
            output "DP-1" {
                mode "3840x2160"
                scale 1.6
                transform "normal"
                position x=0 y=0
                background-color "#2e3440"
                backdrop-color "#434c5e"
                focus-at-startup
            }

            output "eDP-1" {
                mode "1920x1080"
                scale 1
                transform "normal"
                position x=2400 y=0
                background-color "#2e3440"
                backdrop-color "#434c5e"
            }
          '';
      }."${osConfig.networking.hostName}" or "";
  in
    # kdl
    ''
      // This config is in the KDL format: https://kdl.dev
      // "/-" comments out the following node.
      // Check the wiki for a full description of the configuration:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Introduction

      // Input device configuration.
      // Find the full list of options on the wiki:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Input
      input {
          keyboard {
              xkb {
                  layout "us"
              }

              // Enable numlock on startup
              // numlock
          }

          // Next sections include libinput settings.
          // Omitting settings disables them, or leaves them at their default values.
          // All commented-out settings here are examples, not defaults.
          touchpad {
              // off
              tap
              // dwt
              // dwtp
              // drag false
              // drag-lock
              natural-scroll
              // accel-speed 0.2
              // accel-profile "flat"
              // scroll-method "two-finger"
              // disabled-on-external-mouse
          }

          mouse {
              // off
              // natural-scroll
              // accel-speed 0.2
              // accel-profile "flat"
              // scroll-method "no-scroll"
          }

          trackpoint {
              // off
              // natural-scroll
              // accel-speed 0.2
              // accel-profile "flat"
              // scroll-method "on-button-down"
              // scroll-button 273
              // scroll-button-lock
              // middle-emulation
          }

          tablet {
              map-to-output "DP-3"
          }

          // Uncomment this to make the mouse warp to the center of newly focused windows.
          warp-mouse-to-focus

          // Focus windows and outputs automatically when moving the mouse into them.
          // Setting max-scroll-amount="0%" makes it work only on windows already fully on screen.
          focus-follows-mouse max-scroll-amount="0%"

          workspace-auto-back-and-forth
      }

      ${outputs}

      // Settings that influence how windows are positioned and sized.
      // Find more information on the wiki:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Layout
      layout {
          // Set gaps around windows in logical pixels.
          gaps 10

          // When to center a column when changing focus, options are:
          // - "never", default behavior, focusing an off-screen column will keep at the left
          //   or right edge of the screen.
          // - "always", the focused column will always be centered.
          // - "on-overflow", focusing a column will center it if it doesn't fit
          //   together with the previously focused column.
          center-focused-column "never"

          // You can customize the widths that "switch-preset-column-width" (Mod+R) toggles between.
          preset-column-widths {
              // Proportion sets the width as a fraction of the output width, taking gaps into account.
              // For example, you can perfectly fit four windows sized "proportion 0.25" on an output.
              // The default preset widths are 1/3, 1/2 and 2/3 of the output.
              proportion 0.33333
              proportion 0.5
              proportion 0.66667

              // Fixed sets the width in logical pixels exactly.
              // fixed 1920
          }

          // You can also customize the heights that "switch-preset-window-height" (Mod+Shift+R) toggles between.
          // preset-window-heights { }

          // You can change the default width of the new windows.
          default-column-width { proportion 0.5; }
          // If you leave the brackets empty, the windows themselves will decide their initial width.
          // default-column-width {}

          // By default focus ring and border are rendered as a solid background rectangle
          // behind windows. That is, they will show up through semitransparent windows.
          // This is because windows using client-side decorations can have an arbitrary shape.
          //
          // If you don't like that, you should uncomment `prefer-no-csd` below.
          // Niri will draw focus ring and border *around* windows that agree to omit their
          // client-side decorations.
          //
          // Alternatively, you can override it with a window rule called
          // `draw-border-with-background`.

          // You can change how the focus ring looks.
          focus-ring {
              // Uncomment this line to disable the focus ring.
              // off

              // How many logical pixels the ring extends out from the windows.
              width 4

              // Colors can be set in a variety of ways:
              // - CSS named colors: "red"
              // - RGB hex: "#rgb", "#rgba", "#rrggbb", "#rrggbbaa"
              // - CSS-like notation: "rgb(255, 127, 0)", rgba(), hsl() and a few others.

              // Color of the ring on the active monitor.
              // active-color "#7fc8ff"

              // Color of the ring on inactive monitors.
              //
              // The focus ring only draws around the active window, so the only place
              // where you can see its inactive-color is on other monitors.
              // inactive-color "#505050"

              // You can also use gradients. They take precedence over solid colors.
              // Gradients are rendered the same as CSS linear-gradient(angle, from, to).
              // The angle is the same as in linear-gradient, and is optional,
              // defaulting to 180 (top-to-bottom gradient).
              // You can use any CSS linear-gradient tool on the web to set these up.
              // Changing the color space is also supported, check the wiki for more info.
              //
              active-gradient from="#81a1c1" to="#5e81ac" angle=45

              // You can also color the gradient relative to the entire view
              // of the workspace, rather than relative to just the window itself.
              // To do that, set relative-to="workspace-view".
              //
              inactive-gradient from="#8fbcbb" to="#88c0d0" angle=45
          }

          // You can also add a border. It's similar to the focus ring, but always visible.
          border {
              // The settings are the same as for the focus ring.
              // If you enable the border, you probably want to disable the focus ring.
              off

              width 4
              active-color "#ffc87f"
              inactive-color "#505050"

              // Color of the border around windows that request your attention.
              urgent-color "#9b0000"

              // Gradients can use a few different interpolation color spaces.
              // For example, this is a pastel rainbow gradient via in="oklch longer hue".
              //
              // active-gradient from="#e5989b" to="#ffb4a2" angle=45 relative-to="workspace-view" in="oklch longer hue"

              // inactive-gradient from="#505050" to="#808080" angle=45 relative-to="workspace-view"
          }

          // You can enable drop shadows for windows.
          shadow {
              // Uncomment the next line to enable shadows.
              // on

              // By default, the shadow draws only around its window, and not behind it.
              // Uncomment this setting to make the shadow draw behind its window.
              //
              // Note that niri has no way of knowing about the CSD window corner
              // radius. It has to assume that windows have square corners, leading to
              // shadow artifacts inside the CSD rounded corners. This setting fixes
              // those artifacts.
              //
              // However, instead you may want to set prefer-no-csd and/or
              // geometry-corner-radius. Then, niri will know the corner radius and
              // draw the shadow correctly, without having to draw it behind the
              // window. These will also remove client-side shadows if the window
              // draws any.
              //
              // draw-behind-window true

              // You can change how shadows look. The values below are in logical
              // pixels and match the CSS box-shadow properties.

              // Softness controls the shadow blur radius.
              softness 30

              // Spread expands the shadow.
              spread 5

              // Offset moves the shadow relative to the window.
              offset x=0 y=5

              // You can also change the shadow color and opacity.
              color "#0007"
          }

          // Struts shrink the area occupied by windows, similarly to layer-shell panels.
          // You can think of them as a kind of outer gaps. They are set in logical pixels.
          // Left and right struts will cause the next window to the side to always be visible.
          // Top and bottom struts will simply add outer gaps in addition to the area occupied by
          // layer-shell panels and regular gaps.
          struts {
              // left 64
              // right 64
              // top 64
              // bottom 64
          }
      }

      // Add lines like this to spawn processes at startup.
      // Note that running niri as a session supports xdg-desktop-autostart,
      // which may be more convenient to use.
      // See the binds section below for more spawn examples.

      // This line starts waybar, a commonly used bar for Wayland compositors.
      spawn-at-startup "${waybar.packages.${pkgs.system}.waybar}/bin/waybar"
      spawn-at-startup "${swww.packages.${pkgs.system}.default}/bin/swww-daemon"
      spawn-at-startup "${pkgs.networkmanagerapplet}/bin/nm-applet"
      spawn-at-startup "${pkgs.keepassxc}/bin/keepassxc"
      // spawn-at-startup "${pkgs.protonmail-bridge}/bin/protonmail-bridge" "--noninteractive"
      spawn-at-startup "${pkgs.trayscale}/bin/trayscale" "--hide-window"
      spawn-at-startup "${pkgs.nextcloud-client}/bin/nextcloud" "--background"
      spawn-at-startup "~/.config/niri/scripts/wallpaper.fish" "interval" "300"
      spawn-at-startup "${pkgs.niriswitcher}/bin/niriswitcher"
      spawn-at-startup "${pkgs.xwayland-satellite}/bin/xwayland-satellite"

      // Uncomment this line to ask the clients to omit their client-side decorations if possible.
      // If the client will specifically ask for CSD, the request will be honored.
      // Additionally, clients will be informed that they are tiled, removing some client-side rounded corners.
      // This option will also fix border/focus ring drawing behind some semitransparent windows.
      // After enabling or disabling this, you need to restart the apps for this to take effect.
      prefer-no-csd

      // You can change the path where screenshots are saved.
      // A ~ at the front will be expanded to the home directory.
      // The path is formatted with strftime(3) to give you the screenshot date and time.
      screenshot-path "~/Pictures/screenshots/screenshot_%Y-%m-%d_%H-%M-%S.png"

      // You can also set this to null to disable saving screenshots to disk.
      // screenshot-path null

      // Animation settings.
      // The wiki explains how to configure individual animations:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Animations
      animations {
          // Uncomment to turn off all animations.
          // off

          // Slow down all animations by this factor. Values below 1 speed them up instead.
          // slowdown 3.0
      }

      // Window rules let you adjust behavior for individual windows.
      // Find more information on the wiki:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Window-Rules

      window-rule {
        geometry-corner-radius 10
        clip-to-geometry true
      }

      // Work around WezTerm's initial configure bug
      // by setting an empty default-column-width.
      window-rule {
          // This regular expression is intentionally made as specific as possible,
          // since this is the default config, and we want no false positives.
          // You can get away with just app-id="wezterm" if you want.
          match app-id=r#"^org\.wezfurlong\.wezterm$"#
          default-column-width { proportion 0.5; }
      }

      window-rule {
             match app-id=r#"^spotify$"#
             match app-id=r#"^vesktop$"#
             match app-id=r#"^thunderbird$"#
             open-maximized true
         }

      // Open the Firefox picture-in-picture player as floating by default.
      window-rule {
          // This app-id regular expression will work for both:
          // - host Firefox (app-id is "firefox")
          // - Flatpak Firefox (app-id is "org.mozilla.firefox")
          match app-id=r#"firefox$"# title="^Picture-in-Picture$"
          match app-id=r#"librewolf$"# title="^Picture-in-Picture$"
          open-floating true
      }

      window-rule {
          match app-id=r#"^org\.keepassxc\.KeePassXC$"#
          block-out-from "screen-capture"
      }

      window-rule {
          match app-id=r#"qalculate-gtk"#
          open-floating true
          min-height 200
          min-width 400
      }

      window-rule {
          match title=r#"^notificationtoast.*"#
          open-focused false
          default-floating-position x=0 y=0 relative-to="bottom-right"
          geometry-corner-radius 10 0 0 0
          clip-to-geometry true
      }

      window-rule {
          match title="^(Open Files?)(.*)$"
          match app-id=r#"thunar"# title=r#"^(Rename)(.*)$"#
          match title="^KeePassXC - Browser Access Request$"
          match app-id=r#"^org\.keepassxc\.KeePassXC$"# title=r#"Unlock Database - KeePassXC"#
          match app-id=r#"^thunderbird$"# title=r#"^Write"#
          match app-id=r#"^thunderbird$"# title=r#"^New Event"#
          open-floating true
      }

      // Example: enable rounded corners for all windows.
      // (This example rule is commented out with a "/-" in front.)
      /-window-rule {
          geometry-corner-radius 12
          clip-to-geometry true
      }

      binds {
          // Keys consist of modifiers separated by + signs, followed by an XKB key name
          // in the end. To find an XKB name for a particular key, you may use a program
          // like wev.
          //
          // "Mod" is a special modifier equal to Super when running on a TTY, and to Alt
          // when running as a winit window.
          //
          // Most actions that you can bind here can also be invoked programmatically with
          // `niri msg action do-something`.

          // Mod-Shift-/, which is usually the same as Mod-?,
          // shows a list of important hotkeys.
          Mod+Shift+Slash { show-hotkey-overlay; }

          Mod+Return hotkey-overlay-title="Open a Terminal: kitty" { spawn "${pkgs.kitty}/bin/kitty"; }
          Mod+A hotkey-overlay-title="Run an Application: rofi" { spawn "${pkgs.rofi-wayland}/bin/rofi" "-show" "drun"; }
          Mod+Shift+A hotkey-overlay-title="Switch windows: rofi" { spawn "${pkgs.rofi-wayland}/bin/rofi" "-show" "window"; }
          Mod+Z hotkey-overlay-title="Lock the Screen: swaylock" { spawn "~/.config/niri/scripts/lock.fish"; }
          Mod+Control+Z { spawn "${pkgs.wlogout}/bin/wlogout" "-b" "5" "-T" "400" "-B" "400"; }
          Mod+B hotkey-overlay-title="Browser: librewolf" { spawn "${pkgs.librewolf}/bin/librewolf"; }
          Mod+Shift+B hotkey-overlay-title="Browser: firefox" { spawn "${pkgs.firefox}/bin/firefox"; }
          Mod+E { spawn "emacsclient" "-c"; }
          Mod+P hotkey-overlay-title="Password Manager" { spawn "${pkgs.keepassxc}/bin/keepassxc"; }
          Mod+Tab repeat=false { spawn "gdbus" "call" "--session" "--dest" "io.github.isaksamsten.Niriswitcher" "--object-path" "/io/github/isaksamsten/Niriswitcher" "--method" "io.github.isaksamsten.Niriswitcher.application" ; }
          Mod+Shift+Tab repeat=false { spawn "gdbus" "call" "--session" "--dest" "io.github.isaksamsten.Niriswitcher" "--object-path" "/io/github/isaksamsten/Niriswitcher" "--method" "io.github.isaksamsten.Niriswitcher.application" ; }
          Mod+F1 hotkey-overlay-title="File Manager" { spawn "${pkgs.xfce.thunar}/bin/thunar"; }
          Mod+F6 hotkey-overlay-title="Calculator" { spawn "${pkgs.qalculate-gtk}/bin/qalculate-gtk"; }
          Mod+F8 hotkey-overlay-title="Email" { spawn "${pkgs.thunderbird}/bin/thunderbird"; }
          // Needed to get the right, spiced, version from spicetify. Not sure how to reference that from nix
          Mod+F9 hotkey-overlay-title="Music" { spawn "/etc/profiles/per-user/mjs/bin/spotify"; }
          Mod+F10 hotkey-overlay-title="Discord" { spawn "${pkgs.vesktop}/bin/vesktop"; }
          Mod+F12 hotkey-overlay-title="Volume Control" { spawn "${pkgs.pavucontrol}/bin/pavucontrol"; }

          // You can also use a shell. Do this if you need pipes, multiple commands, etc.
          // Note: the entire command goes as a single argument in the end.
          // Mod+T { spawn "bash" "-c" "notify-send hello && exec alacritty"; }

          // Example volume keys mappings for PipeWire & WirePlumber.
          XF86AudioPlay        allow-when-locked=true { spawn "${pkgs.playerctl}/bin/playerctl" "play-pause"; }
          XF86AudioNext                               { spawn "${pkgs.playerctl}/bin/playerctl" "next"; }
          XF86AudioPrev                               { spawn "${pkgs.playerctl}/bin/playerctl" "previous"; }
          XF86AudioStop        allow-when-locked=true { spawn "${pkgs.playerctl}/bin/playerctl" "stop"; }
          XF86AudioRaiseVolume allow-when-locked=true { spawn "${pkgs.swayosd}/bin/swayosd-client" "--output-volume" "raise"; }
          XF86AudioLowerVolume allow-when-locked=true { spawn "${pkgs.swayosd}/bin/swayosd-client" "--output-volume" "lower"; }
          XF86AudioMute        allow-when-locked=true { spawn "${pkgs.swayosd}/bin/swayosd-client" "--output-volume" "mute-toggle"; }
          XF86AudioMicMute     allow-when-locked=true { spawn "${pkgs.swayosd}/bin/swayosd-client" "--input-volume" "mute-toggle"; }

          // Example brightness key mappings for brightnessctl.
          XF86MonBrightnessUp   allow-when-locked=true { spawn "${pkgs.swayosd}/bin/swayosd-client" "--brightness" "raise"; }
          XF86MonBrightnessDown allow-when-locked=true { spawn "${pkgs.swayosd}/bin/swayosd-client" "--brightness" "lower"; }

          // Open/close the Overview: a zoomed-out view of workspaces and windows.
          // You can also move the mouse into the top-left hot corner,
          // or do a four-finger swipe up on a touchpad.
          Mod+O repeat=false { toggle-overview; }

          Mod+Q repeat=false { close-window; }

          Mod+Left  { focus-column-left; }
          Mod+Down  { focus-window-down; }
          Mod+Up    { focus-window-up; }
          Mod+Right { focus-column-right; }
          Mod+H     { focus-column-left; }
          Mod+J     { focus-window-down; }
          Mod+K     { focus-window-up; }
          Mod+L     { focus-column-right; }

          Mod+Ctrl+Left  { move-column-left; }
          Mod+Ctrl+Down  { move-window-down; }
          Mod+Ctrl+Up    { move-window-up; }
          Mod+Ctrl+Right { move-column-right; }
          Mod+Ctrl+H     { move-column-left; }
          Mod+Ctrl+J     { move-window-down; }
          Mod+Ctrl+K     { move-window-up; }
          Mod+Ctrl+L     { move-column-right; }

          // Alternative commands that move across workspaces when reaching
          // the first or last window in a column.
          // Mod+J     { focus-window-or-workspace-down; }
          // Mod+K     { focus-window-or-workspace-up; }
          // Mod+Ctrl+J     { move-window-down-or-to-workspace-down; }
          // Mod+Ctrl+K     { move-window-up-or-to-workspace-up; }

          Mod+Home { focus-column-first; }
          Mod+End  { focus-column-last; }
          Mod+Ctrl+Home { move-column-to-first; }
          Mod+Ctrl+End  { move-column-to-last; }

          Mod+Shift+Left  { focus-monitor-left; }
          Mod+Shift+Down  { focus-monitor-down; }
          Mod+Shift+Up    { focus-monitor-up; }
          Mod+Shift+Right { focus-monitor-right; }
          Mod+Shift+H     { focus-monitor-left; }
          Mod+Shift+J     { focus-monitor-down; }
          Mod+Shift+K     { focus-monitor-up; }
          Mod+Shift+L     { focus-monitor-right; }

          Mod+Shift+Ctrl+Left  { move-column-to-monitor-left; }
          Mod+Shift+Ctrl+Down  { move-column-to-monitor-down; }
          Mod+Shift+Ctrl+Up    { move-column-to-monitor-up; }
          Mod+Shift+Ctrl+Right { move-column-to-monitor-right; }
          Mod+Shift+Ctrl+H     { move-column-to-monitor-left; }
          Mod+Shift+Ctrl+J     { move-column-to-monitor-down; }
          Mod+Shift+Ctrl+K     { move-column-to-monitor-up; }
          Mod+Shift+Ctrl+L     { move-column-to-monitor-right; }

          // Alternatively, there are commands to move just a single window:
          // Mod+Shift+Ctrl+Left  { move-window-to-monitor-left; }
          // ...

          // And you can also move a whole workspace to another monitor:
          // Mod+Shift+Ctrl+Left  { move-workspace-to-monitor-left; }
          // ...

          Mod+Page_Down      { focus-workspace-down; }
          Mod+Page_Up        { focus-workspace-up; }
          Mod+U              { focus-workspace-down; }
          Mod+I              { focus-workspace-up; }
          Mod+Ctrl+Page_Down { move-column-to-workspace-down; }
          Mod+Ctrl+Page_Up   { move-column-to-workspace-up; }
          Mod+Ctrl+U         { move-column-to-workspace-down; }
          Mod+Ctrl+I         { move-column-to-workspace-up; }

          // Alternatively, there are commands to move just a single window:
          // Mod+Ctrl+Page_Down { move-window-to-workspace-down; }
          // ...

          Mod+Shift+Page_Down { move-workspace-down; }
          Mod+Shift+Page_Up   { move-workspace-up; }
          Mod+Shift+U         { move-workspace-down; }
          Mod+Shift+I         { move-workspace-up; }

          // You can bind mouse wheel scroll ticks using the following syntax.
          // These binds will change direction based on the natural-scroll setting.
          //
          // To avoid scrolling through workspaces really fast, you can use
          // the cooldown-ms property. The bind will be rate-limited to this value.
          // You can set a cooldown on any bind, but it's most useful for the wheel.
          Mod+WheelScrollDown      cooldown-ms=150 { focus-workspace-down; }
          Mod+WheelScrollUp        cooldown-ms=150 { focus-workspace-up; }
          Mod+Ctrl+WheelScrollDown cooldown-ms=150 { move-column-to-workspace-down; }
          Mod+Ctrl+WheelScrollUp   cooldown-ms=150 { move-column-to-workspace-up; }

          Mod+WheelScrollRight      { focus-column-right; }
          Mod+WheelScrollLeft       { focus-column-left; }
          Mod+Ctrl+WheelScrollRight { move-column-right; }
          Mod+Ctrl+WheelScrollLeft  { move-column-left; }

          // Usually scrolling up and down with Shift in applications results in
          // horizontal scrolling; these binds replicate that.
          Mod+Shift+WheelScrollDown      { focus-column-right; }
          Mod+Shift+WheelScrollUp        { focus-column-left; }
          Mod+Ctrl+Shift+WheelScrollDown { move-column-right; }
          Mod+Ctrl+Shift+WheelScrollUp   { move-column-left; }

          // Similarly, you can bind touchpad scroll "ticks".
          // Touchpad scrolling is continuous, so for these binds it is split into
          // discrete intervals.
          // These binds are also affected by touchpad's natural-scroll, so these
          // example binds are "inverted", since we have natural-scroll enabled for
          // touchpads by default.
          // Mod+TouchpadScrollDown { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02+"; }
          // Mod+TouchpadScrollUp   { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02-"; }

          // You can refer to workspaces by index. However, keep in mind that
          // niri is a dynamic workspace system, so these commands are kind of
          // "best effort". Trying to refer to a workspace index bigger than
          // the current workspace count will instead refer to the bottommost
          // (empty) workspace.
          //
          // For example, with 2 workspaces + 1 empty, indices 3, 4, 5 and so on
          // will all refer to the 3rd workspace.
          Mod+1 { focus-workspace 1; }
          Mod+2 { focus-workspace 2; }
          Mod+3 { focus-workspace 3; }
          Mod+4 { focus-workspace 4; }
          Mod+5 { focus-workspace 5; }
          Mod+6 { focus-workspace 6; }
          Mod+7 { focus-workspace 7; }
          Mod+8 { focus-workspace 8; }
          Mod+9 { focus-workspace 9; }
          Mod+Ctrl+1 { move-column-to-workspace 1; }
          Mod+Ctrl+2 { move-column-to-workspace 2; }
          Mod+Ctrl+3 { move-column-to-workspace 3; }
          Mod+Ctrl+4 { move-column-to-workspace 4; }
          Mod+Ctrl+5 { move-column-to-workspace 5; }
          Mod+Ctrl+6 { move-column-to-workspace 6; }
          Mod+Ctrl+7 { move-column-to-workspace 7; }
          Mod+Ctrl+8 { move-column-to-workspace 8; }
          Mod+Ctrl+9 { move-column-to-workspace 9; }

          // Alternatively, there are commands to move just a single window:
          // Mod+Ctrl+1 { move-window-to-workspace 1; }

          // Switches focus between the current and the previous workspace.
          // Mod+Tab { focus-workspace-previous; }

          // The following binds move the focused window in and out of a column.
          // If the window is alone, they will consume it into the nearby column to the side.
          // If the window is already in a column, they will expel it out.
          Mod+BracketLeft  { consume-or-expel-window-left; }
          Mod+BracketRight { consume-or-expel-window-right; }

          // Consume one window from the right to the bottom of the focused column.
          Mod+Comma  { consume-window-into-column; }
          // Expel the bottom window from the focused column to the right.
          Mod+Period { expel-window-from-column; }

          Mod+R { switch-preset-column-width; }
          Mod+Shift+R { switch-preset-window-height; }
          Mod+Ctrl+R { reset-window-height; }
          Mod+F { maximize-column; }
          Mod+Shift+F { fullscreen-window; }

          // Expand the focused column to space not taken up by other fully visible columns.
          // Makes the column "fill the rest of the space".
          Mod+Ctrl+F { expand-column-to-available-width; }

          Mod+C { center-column; }

          // Center all fully visible columns on screen.
          Mod+Ctrl+C { center-visible-columns; }

          // Finer width adjustments.
          // This command can also:
          // * set width in pixels: "1000"
          // * adjust width in pixels: "-5" or "+5"
          // * set width as a percentage of screen width: "25%"
          // * adjust width as a percentage of screen width: "-10%" or "+10%"
          // Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
          // set-column-width "100" will make the column occupy 200 physical screen pixels.
          Mod+Minus { set-column-width "-10%"; }
          Mod+Equal { set-column-width "+10%"; }

          // Finer height adjustments when in column with other windows.
          Mod+Shift+Minus { set-window-height "-10%"; }
          Mod+Shift+Equal { set-window-height "+10%"; }

          // Move the focused window between the floating and the tiling layout.
          Mod+V       { toggle-window-floating; }
          Mod+Shift+V { switch-focus-between-floating-and-tiling; }

          // Toggle tabbed column display mode.
          // Windows in this column will appear as vertical tabs,
          // rather than stacked on top of each other.
          Mod+W { toggle-column-tabbed-display; }

          // Actions to switch layouts.
          // Note: if you uncomment these, make sure you do NOT have
          // a matching layout switch hotkey configured in xkb options above.
          // Having both at once on the same hotkey will break the switching,
          // since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
          // Mod+Space       { switch-layout "next"; }
          // Mod+Shift+Space { switch-layout "prev"; }

          Print { screenshot; }
          Ctrl+Print { screenshot-screen; }
          Alt+Print { screenshot-window; }

          // Applications such as remote-desktop clients and software KVM switches may
          // request that niri stops processing the keyboard shortcuts defined here
          // so they may, for example, forward the key presses as-is to a remote machine.
          // It's a good idea to bind an escape hatch to toggle the inhibitor,
          // so a buggy application can't hold your session hostage.
          //
          // The allow-inhibiting=false property can be applied to other binds as well,
          // which ensures niri always processes them, even when an inhibitor is active.
          Mod+Escape allow-inhibiting=false { toggle-keyboard-shortcuts-inhibit; }

          // The quit action will show a confirmation dialog to avoid accidental exits.
          Mod+Shift+E { quit; }
          Ctrl+Alt+Delete { quit; }

          // Powers off the monitors. To turn them back on, do any input like
          // moving the mouse or pressing any other key.
          Mod+Shift+P { power-off-monitors; }

      }
    '';
}

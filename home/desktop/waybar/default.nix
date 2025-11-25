{
  osConfig,
  config,
  lib,
  pkgs,
  waybar,
  ...
}: {
  programs.waybar = {
    enable = true;
    package = waybar.packages.${pkgs.stdenv.hostPlatform.system}.waybar;
    systemd.enable = false;
    settings = {
      bar = {
        layer = "top";
        position = "top";
        height = 37;
        margin = "0 0 0 0";
        output =
          {
            "terra" = ["DP-3" "DP-4"];
            "venus" = ["eDP-1" "DP-1"];
            "mars" = ["eDP-1" "DP-1" "HDMI-A-1"];
            "luna" = ["eDP-1" "HDMI-A-2"];
          }
          ."${osConfig.networking.hostName}"
          or [
          ];
        modules-left = ["niri/workspaces" "wlr/taskbar" "tray" "mpris"];
        modules-center = ["niri/window"];
        modules-right = [
          "privacy"
          "clock"
          "memory"
          "cpu"
          "network"
          "bluetooth"
          "battery"
          "idle_inhibitor"
        ];
        "niri/workspaces" = {
          all-outputs = false;
          format = "{icon}";
          format-icons = {
            "active" = "";
            "focused" = "";
            "default" = "";
          };
        };
        "niri/window" = {
          format = "{title:.50}";
          separate-outputs = true;
          icon = true;
        };
        "wlr/taskbar" = {
          format = "{icon}";
          icon-size = 15;
          tooltip-format = "{title}";
          on-click = "activate";
          on-click-right = "close";
        };
        "tray" = {
          icon-size = 15;
          spacing = 5;
        };
        "mpris" = {
          format = "{player_icon} - {title}";
          format-paused = "{player_icon} - {status_icon} - {title}";
          title-len = 20;
          player-icons = {
            spotify = " ";
            firefox = "󰈹 ";
            brave = "󰄛 ";
            default = " ";
          };
          status-icons = {
            paused = "";
          };
        };
        "clock" = {
          format = "  {:%H:%M 󰃭 %a %b %d %Y}";
          timezone = "America/Chicago";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "month";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            on-click-right = "mode";
            format = {
              months = "<span color='${config.lib.stylix.colors.withHashtag.base0E}'><b>{}</b></span>";
              days = "<span color='${config.lib.stylix.colors.withHashtag.base05}'><b>{}</b></span>";
              weeks = "<span color='${config.lib.stylix.colors.withHashtag.base0D}'><b>W{}</b></span>";
              weekdays = "<span color='${config.lib.stylix.colors.withHashtag.base08}'><b>{}</b></span>";
              today = "<span color='${config.lib.stylix.colors.withHashtag.base09}'><b><u>{}</u></b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-click-forward = "tz_up";
            on-click-backward = "tz_down";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };
        "privacy" = {
          icon-spacing = 5;
          icon-size = 20;
          transition-duraiton = 250;
          modules = [
            {
              type = "screenshare";
              tooltip = true;
              tooltip-icon-size = 20;
            }
            {
              type = "audio-in";
              tooltip = true;
              tooltip-icon-size = 20;
            }
            {
              type = "audio-out";
              tooltip = true;
              tooltip-icon-size = 20;
            }
          ];
        };
        "memory" = {
          format = "  {percentage}% 󰓡 {swapPercentage}%";
          tooltip = true;
          tooltip-format = "{used:0.1f} GiB Used, {swapUsed:0.1f} GiB Swap";
        };
        "cpu" = {
          interval = 5;
          format = "{icon} {usage}%";
          format-icons = [
            "<span color='${config.lib.stylix.colors.withHashtag.base0C}'> </span>"
            "<span color='${config.lib.stylix.colors.withHashtag.base0C}'> </span>"
            "<span color='${config.lib.stylix.colors.withHashtag.base0A}'> </span>"
            "<span color='${config.lib.stylix.colors.withHashtag.base0A}'> </span>"
            "<span color='${config.lib.stylix.colors.withHashtag.base09}'> </span>"
            "<span color='${config.lib.stylix.colors.withHashtag.base09}'> </span>"
            "<span color='${config.lib.stylix.colors.withHashtag.base08}'> </span>"
            "<span color='${config.lib.stylix.colors.withHashtag.base08}'> </span>"
          ];
          tooltip = true;
          tooltip-format = "{avg_frequency} GHz";
        };
        "network" = {
          format-wifi = "{icon}";
          format-ethernet = "   {bandwidthUpOctets}  {bandwidthDownOctets}";
          tooltip = true;
          tooltip-format-wifi = "{essid}  {bandwidthUpOctets}  {bandwidthDownOctets}";
          format-icons = [
            "󰤯 "
            "󰤟 "
            "󰤢 "
            "󰤥 "
            "󰤨 "
          ];
        };
        "bluetooth" = {
          format = "";
          format-disabled = "󰂲";
          format-off = "󰂲";
          format-on = "";
          format-connected = "󰂱";
          tooltip-format = "{controller_alias}\t{controller_address}\n\n{num_connections} connected";
          tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{num_connections} connected\n\n{device_enumerate}";
          tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
          tooltip-format-enumerate-connected-battery = "{device_alias}\t{device_address}\t{device_battery_percentage}%";
        };
        "battery" = {
          interval = 60;
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{icon}  {capacity}%";
          format-icons = ["" "" "" "" ""];
        };
        "idle_inhibitor" = {
          format = "{icon}";
          format-icons = {
            activated = " ";
            deactivated = "󰒲 ";
          };
        };
      };
    };
    # Style for all devices
    style =
      lib.mkAfter
      # css
      ''
        * {
            color: @base04;
            border: 0;
            padding: 0 0;
        }

        window#waybar {
            background-color: transparent;
        }

        #workspaces {
            background-color: @base01;
            margin: 5px;
            border-radius: 15px;
            padding: 0px 15px;
        }

        #workspaces button {
            padding: 0px 5px;
            font-size: 20px;
            color: @base0E;
        }

        #workspaces button.active {
            color: @base0E;
        }

        #workspaces button.focused {
            color: @base09;
        }

        #workspaces button.empty {
            color: @base0B;
        }

        #tray {
            background-color: @base01;
            border-radius: 15px 15px 15px 15px;
            padding: 0px 15px;
            margin: 5px;
        }

        #mode {
            background-color: @base01;
            border-radius: 15px;
            padding: 0px 15px;
            margin: 5px;
        }

        #taskbar {
            background-color: @base01;
            border-radius: 15px;
            padding: 0px 15px;
            margin: 5px;
        }

        #taskbar button {
            padding: 0px 5px;
        }

        #taskbar.empty {
            background-color: transparent;
            border-radius: 0px;
            padding: 0px 0px;
            margin: 0px;
        }

        #mpris {
            background-color: @base01;
            border-radius: 15px;
            padding: 0px 15px;
            margin: 5px;
        }

        #mpris.spotify {
            color: @base0B;
        }

        #window {
            margin: 5px;
            background-color: @base01;
            padding: 0px 15px;
            border-radius: 15px;
        }

        window#waybar.empty #window {
        	background: rgba(46, 52, 64, 0.0);
        }

        .modules-right {
            background-color: @base01;
            margin: 5px;
            border-radius: 15px;
            padding: 0px 15px 0px 15px;
        }

        #privacy {
            background-color: @base01;
            padding: 0px 5px 0px 0px;
        }

        #clock {
            background-color: @base01;
            color: @base0D;
            padding: 0px 5px 0px 5px;
        }

        #memory {
            background-color: @base01;
            color: @base0E;
            padding: 0px 5px 0px 5px;
        }

        #cpu {
            background-color: @base01;
            padding: 0px 5px 0px 5px;
        }

        #network {
            background-color: @base01;
            color: @base0C;
            padding: 0px 5px 0px 5px;
        }

        #bluetooth {
            background-color: @base01;
            color: @base0D;
            padding: 0px 5px 0px 5px;
        }

        #battery {
            background-color: @base01;
            padding: 0px 5px 0px 5px;
        }

        #battery.warning {
            background-color: @base09;
            color: @base01;
        }

        #battery.critical {
            background-color: @base08;
            color: @base01;
        }

        #idle_inhibitor {
            background-color: @base01;
            color: @base09;
            padding: 0px 0px 0px 5px;
        }
      '';
  };

  stylix.targets.waybar = {
    addCss = false;
  };
}

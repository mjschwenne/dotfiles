{
  osConfig,
  pkgs,
  waybar,
  ...
}: {
  programs.waybar = {
    enable = true;
    package = waybar.packages.${pkgs.system}.waybar;
    systemd.enable = false;
    settings = {
      bar = {
        layer = "top";
        position = "top";
        height = 30;
        margin = "0 0 0 0";
        output =
          {
            "terra" = ["HDMI-A-5" "DP-3"];
            # TODO update with correct second monitor names
            "mars" = ["eDP-1" "HDMI-1"];
            "luna" = ["eDP-1" "HDMI-1"];
          }
          ."${osConfig.networking.hostName}"
          or [];
        # Basically, I need hyprland on terra and sway everywhere else
        modules-left =
          {
            "terra" = ["hyprland/workspaces"];
            "mars" = ["sway/workspaces" "sway/mode"];
            "luna" = ["sway/workspaces" "sway/mode"];
          }
          ."${osConfig.networking.hostName}"
          or []
          ++ ["tray" "mpris"];
        modules-center =
          if osConfig.networking.hostName == "terra"
          then ["hyprland/window"]
          else ["sway/window"];
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
        "sway/workspaces" = {
          disable-scroll = true;
          all-outputs = true;
          format = "{icon}";
          format-icons = {
            "1" = "󰲡";
            "2" = "󰲣";
            "3" = "󰲥";
            "4" = "󰲧";
            "5" = "󰲩";
            "6" = "󰲫";
            "7" = "󰲭";
            "8" = "󰲯";
            "9" = "󰲱";
            "10" = "󰿭";
          };
          persistent-workspaces = {
            "1" = [];
            "2" = [];
            "3" = [];
            "4" = [];
            "5" = [];
            "6" = [];
            "7" = [];
            "8" = [];
            "9" = [];
            "10" = [];
          };
        };
        "sway/mode" = {
          format = ''<span style="italic">{}</span>'';
          max-length = 15;
        };
        "sway/window" = {
          format = "{title}";
          max-length = 50;
        };
        "hyprland/workspaces" = {
          all-outputs = true;
          format = "{icon}";
          format-icons = {
            "1" = "󰲡";
            "2" = "󰲣";
            "3" = "󰲥";
            "4" = "󰲧";
            "5" = "󰲩";
            "6" = "󰲫";
            "7" = "󰲭";
            "8" = "󰲯";
            "9" = "󰲱";
            "10" = "󰿭";
          };
          persistent-workspaces = {
            "1" = [];
            "2" = [];
            "3" = [];
            "4" = [];
            "5" = [];
            "6" = [];
            "7" = [];
            "8" = [];
            "9" = [];
            "10" = [];
          };
        };
        "hyprland/window" = {
          format = "{title}";
          max-length = 50;
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
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
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
            "<span color='#a6e3a1'> </span>"
            "<span color='#a6e3a1'> </span>"
            "<span color='#f9e2af'> </span>"
            "<span color='#f9e2af'> </span>"
            "<span color='#fab387'> </span>"
            "<span color='#fab387'> </span>"
            "<span color='#f38ba8'> </span>"
            "<span color='#f38ba8'> </span>"
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
          format = "{capacity}% {icon}";
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
    style = let
      base = "#1e1e2e";
      mantle = "#181825";
      crust = "#11111b";
      text = "#cdd6f4";
      subtext0 = "#a6adc8";
      subtext1 = "#bac2de";
      surface0 = "#313244";
      surface1 = "#45475a";
      surface2 = "#585b70";
      overlay0 = "#6c7086";
      overlay1 = "#7f849c";
      overlay2 = "#9399b2";
      blue = "#89b4fa";
      lavender = "#b4befe";
      sapphire = "#74c7ec";
      sky = "#89dceb";
      teal = "#94e2d5";
      green = "#a6e3a1";
      yellow = "#f9e2af";
      peach = "#fab387";
      maroon = "#eba0ac";
      red = "#f38ba8";
      mauve = "#cba6f7";
      pink = "#f5c2e7";
      flamingo = "#f2cdcd";
      rosewater = "#f5e0dc";
    in
      /*
      css
      */
      ''
        * {
            color: ${text};
            border: 0;
            padding: 0 0;
            font-family: "JetBrains Mono Nerd Font";
        }

        window#waybar {
            background-color: transparent;
        }

        window#waybar.solo {
            /* Mocha, but translucent */
            background-color: alpha(${mantle}, 0.7);
        }

        #workspaces {
            background-color: ${base};
            margin: 5px;
            border-radius: 15px;
            padding: 0px 15px;
        }

        #workspaces button {
            border-radius: 0px;
            background-color: ${base};
            color: ${peach};
            border-bottom: 3px solid ${peach};
            ${
          if osConfig.networking.hostName == "terra"
          then "padding: 0px 10px;"
          else ""
        }
        }

        #workspaces button.visible {
            color: ${yellow};
            border-bottom: 3px solid ${yellow};
        }

        #workspaces button.active,
        #workspaces button.focused {
            color: ${blue};
            border-bottom: 3px solid ${blue};
        }

        ${
          if osConfig.networking.hostName == "terra"
          then "#workspaces button.empty"
          else "#workspaces button.persistent"
        } {
            color: ${text};
            border-bottom: 3px solid ${base};
        }

        #tray {
            background-color: ${base};
            border-radius: 15px 15px 15px 15px;
            padding: 0px 15px;
            margin: 5px;
        }

        #mode {
            background-color: ${base};
            border-radius: 15px;
            padding: 0px 15px;
            margin: 5px;
        }

        #mpris {
            background-color: ${base};
            border-radius: 15px;
            padding: 0px 15px;
            margin: 5px;
        }

        #mpris.spotify {
            color: ${green};
        }

        #window {
            margin: 5px;
            background-color: ${base};
            padding: 0px 15px;
            border-radius: 15px;
        }

        window#waybar.empty #window {
        	background: rgba(12, 12, 12, 0.0);
        }

        .modules-right {
            background-color: ${base};
            margin: 5px;
            border-radius: 15px;
            padding: 0px 15px 0px 15px;
        }

        @keyframes blink {
            to {
                background-color: #ffffff;
                color: black;
            }
        }

        #privacy {
            background-color: ${base};
            padding: 0px 5px 0px 0px;
        }

        #clock {
            background-color: ${base};
            color: ${mauve};
            padding: 0px 5px 0px 5px;
        }

        #memory {
            background-color: ${base};
            color: ${sky};
            padding: 0px 5px 0px 5px;
        }

        #cpu {
            background-color: ${base};
            padding: 0px 5px 0px 5px;
        }

        #network {
            background-color: ${base};
            color: ${pink};
            padding: 0px 5px 0px 5px;
        }

        #bluetooth {
            background-color: ${base};
            color: ${blue};
            padding: 0px 5px 0px 5px;
        }

        #battery {
            background-color: ${base};
            padding: 0px 5px 0px 5px;
        }

        #idle_inhibitor {
            background-color: ${base};
            color: ${peach};
            padding: 0px 0px 0px 5px;
        }
      '';
  };
}

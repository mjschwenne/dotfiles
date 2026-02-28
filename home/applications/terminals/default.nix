{
  pkgs,
  osConfig,
  ...
}@inputs:
{

  xdg.terminal-exec = {
    enable = true;
    settings = {
      default = [
        "ghostty.desktop"
      ];
    };
  };
  programs = {
    foot = {
      enable = true;
      settings = {
        main = {
          shell = "fish";
          term = "xterm-256color";
          title = "foot";
          letter-spacing = 0;
          pad = "5x5 center";
        };
        scrollback = {
          lines = 10000;
        };
        cursor.style = "block";
      };
    };

    kitty = {
      enable = false;
      shellIntegration = {
        enableFishIntegration = true;
        mode = "no-cursor";
      };
      settings = {
        disable_ligatures = "never";
        cursor_shape = "block";
        share_connections = "no";
      };
    };

    ghostty = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        cursor-style = "block";
        cursor-style-blink = false;
        shell-integration-features = "no-cursor,ssh-env";
      };
    };

    wezterm = {
      enable = true;
      package = inputs.wezterm.packages.${pkgs.stdenv.hostPlatform.system}.default;
      extraConfig =
        let
          preferred_adapter = {
            "terra" =
              # lua
              ''
                webgpu_preferred_adapter = {
                  backend = "Vulkan",
                  device = 29822,
                  device_type = "DiscreteGpu",
                  driver = "radv",
                  driver_info = "Mesa 24.1.1",
                  name = "AMD Radeon RX 7800 XT (RADV NAVI32)",
                  vendor = 4098,
                },
              '';
            "mars" =
              # lua
              ''
                webgpu_preferred_adapter = {
                  backend = "Vulkan",
                  device = 26880,
                  device_type = "DiscreteGpu",
                  driver = "radv",
                  driver_info = "Mesa 24.0.5",
                  name = "AMD Radeon R5 M465 Series (RADV ICELAND)",
                  vendor = 4098,
                },
              '';
            "venus" =
              # lua
              ''
                webgpu_preferred_adapter = {
                      backend = "Vulkan",
                      device = 32069,
                      device_type = "IntegratedGpu",
                      driver_info = "Mesa 25.0.1",
                      name = "Intel(R) Graphics (MTL)",
                      vendor = 32902
                },
              '';
            "luna" =
              # lua
              ''
                webgpu_preferred_adapter = {
                      backend = "Vulkan",
                      device = 26880,
                      device_type = "DiscreteGpu",
                      driver = "radv",
                      driver_info = "Mesa 24.0.5",
                      name = "AMD Radeon R5 M465 Series (RADV ICELAND)",
                      vendor = 4098,
                },
              '';
            # Copied from Terra to suppress error.
            # Not planning on using wezterm on sol.
            "sol" = /* lua */ ''
              webgpu_preferred_adapter = {
                backend = "Vulkan",
                device = 29822,
                device_type = "DiscreteGpu",
                driver = "radv",
                driver_info = "Mesa 24.1.1",
                name = "AMD Radeon RX 7800 XT (RADV NAVI32)",
                vendor = 4098,
              },
            '';
          };
        in
        /* lua */ ''
          local wezterm = require 'wezterm'
          return {
            front_end = "WebGpu",
            ${preferred_adapter."${osConfig.networking.hostName}"}
            font_size = 12.0,
            enable_tab_bar = false,
            default_prog = { "fish", "-l" },
            enable_wayland = true,
            check_for_updates = false,
            keys = {
              {key="Enter", mods="SHIFT", action=wezterm.action{SendString="\x1b\r"}},
            },
          }
        '';
    };
  };
}

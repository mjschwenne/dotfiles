{
  pkgs,
  osConfig,
  ...
} @ inputs: {
  programs = {
    alacritty = {
      enable = true;
      catppuccin.enable = true;
      settings = {
        shell = "fish";
        env = {
          TERM = "xterm-256color";
        };
        window = {
          dynamic_padding = true;
          padding = {
            x = 5;
            y = 5;
          };
        };
        font = {
          normal = {
            family = "JetBrainsMono Nerd Font";
            style = "Regular";
          };
          bold = {
            family = "JetBrainsMono Nerd Font";
            style = "Bold";
          };
          italic = {
            family = "JetBrainsMono Nerd Font";
            style = "Italic";
          };
          bold_italic = {
            family = "JetBrainsMono Nerd Font";
            style = "Bold Italic";
          };
        };
      };
    };

    foot = {
      enable = true;
      catppuccin.enable = true;
      settings = {
        main = {
          shell = "fish";
          term = "xterm-256color";
          title = "foot";
          font = "JetBrainsMono Nerd Font:size=11:fontfeatures=calt:fontfeatures=dlig:fontfeatures=liga";
          letter-spacing = 0;
          pad = "5x5 center";
        };
        scrollback = {
          lines = 10000;
        };
        cursor = {
          style = "block";
        };
      };
    };

    kitty = {
      enable = true;
      catppuccin.enable = true;
      font.name = "JetBrainsMono Nerd Font";
      shellIntegration.mode = "no-cursor";
      settings = {
        disable_ligatures = "never";
        cursor_shape = "block";
        share_connections = "no";
      };
    };

    wezterm = {
      enable = true;
      package = inputs.wezterm.packages.${pkgs.system}.default;
      extraConfig = let
        preferred_adapter = {
          "terra" =
            /*
            lua
            */
            ''
              webgpu_preferred_adapter = {
                backend = "Vulkan",
                device = 10118,
                device_type = "DiscreteGpu",
                driver = "NVIDIA",
                driver_info = "550.78",
                name = "NVIDIA GeForce RTX 4070",
                vendor = 4318,
              },
            '';
          "mars" =
            /*
            lua
            */
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
          "luna" =
            /*
            lua
            */
            ''
              webgpu_preferred_adapter = {
                    backend = "Vulkan",
                    device = 26880,
                    device_type = "DiscreteGpu",
                    driver = "radv",
                    driver_info = "Mesa 24.0.5",
                    name = "AMD Radeon R5 M465 Series (RADV ICELAND)",
                    vendor = 4098,
              }
            '';
        };
      in
        /*
        lua
        */
        ''
          local wezterm = require 'wezterm'
          return {
            front_end = "WebGpu",
            ${preferred_adapter."${osConfig.networking.hostName}"}
            font = wezterm.font("JetBrainsMono Nerd Font", {weight = "Light"}),
            font_size = 11.0,
            color_scheme = "Catppuccin Mocha",
            window_background_opacity = 1.0,
            hide_tab_bar_if_only_one_tab = true,
            default_prog = { "fish", "-l" },
            enable_wayland = true,
          }
        '';
    };
  };

  xdg.configFile."alacritty/rose-pine.toml".source = ./rose-pine.toml;
}

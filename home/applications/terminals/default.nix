{
  pkgs,
  osConfig,
  ...
} @ inputs: {
  programs = {
    alacritty = {
      enable = true;
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
        import = ["~/.config/alacritty/catppuccin-mocha.toml"];
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
        colors = {
          alpha = 0.8;
          foreground = "cdd6f4"; # text
          background = "1e1e2e"; # base
          regular0 = "45475a"; # surface 1
          regular1 = "f38ba8"; # red
          regular2 = "a6e3a1"; # green
          regular3 = "f9e2af"; # yellow
          regular4 = "89b4fa"; # blue
          regular5 = "f5c2e7"; # pink
          regular6 = "94e2d5"; # teal
          regular7 = "bac2de"; # subtext 1
          bright0 = "585b70"; # surface 2
          bright1 = "f38ba8"; # red
          bright2 = "a6e3a1"; # green
          bright3 = "f9e2af"; # yellow
          bright4 = "89b4fa"; # blue
          bright5 = "f5c2e7"; # pink
          bright6 = "94e2d5"; # teal
          bright7 = "a6adc8"; # subtext 0
        };
      };
    };

    kitty = {
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
            window_background_opacity = 0.8,
            hide_tab_bar_if_only_one_tab = true,
            default_prog = { "fish", "-l" },
            enable_wayland = true,
          }
        '';
    };
  };

  xdg.configFile."alacritty/catppuccin-mocha.toml".source = ./alacritty-catppuccin-mocha.toml;
}

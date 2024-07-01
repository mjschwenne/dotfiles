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
        import = ["~/.config/alacritty/rose-pine.toml"];
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
          color = "191724 e0def4";
        };
        colors = {
          background = "191724";
          foreground = "e0def4";
          regular0 = "26233a"; # black (Overlay)
          regular1 = "eb6f92"; # red (Love)
          regular2 = "31748f"; # green (Pine)
          regular3 = "f6c177"; # yellow (Gold)
          regular4 = "9ccfd8"; # blue (Foam)
          regular5 = "c4a7e7"; # magenta (Iris)
          regular6 = "ebbcba"; # cyan (Rose)
          regular7 = "e0def4"; # white (Text)

          bright0 = "6e6a86"; # bright black (Overlay)
          bright1 = "eb6f92"; # bright red (Love)
          bright2 = "31748f"; # bright green (Pine)
          bright3 = "f6c177"; # bright yellow (Gold)
          bright4 = "9ccfd8"; # bright blue (Foam)
          bright5 = "c4a7e7"; # bright magenta (Iris)
          bright6 = "ebbcba"; # bright cyan (Rose)
          bright7 = "e0def4"; # bright white (Text)alpha = 1.0;
        };
      };
    };

    kitty = {
      enable = true;
      theme = "Rosé Pine";
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
            color_scheme = "rose-pine",
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

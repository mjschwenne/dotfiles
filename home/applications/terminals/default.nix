{
  pkgs,
  osConfig,
  ...
} @ inputs: {
  programs = {
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
        colors.alpha = 0.8;
        cursor = {
          style = "block";
        };
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
                device = 29822,
                device_type = "DiscreteGpu",
                driver = "radv",
                driver_info = "Mesa 24.1.1",
                name = "AMD Radeon RX 7800 XT (RADV NAVI32)",
                vendor = 4098,
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
            window_background_opacity = 0.8,
            hide_tab_bar_if_only_one_tab = true,
            default_prog = { "fish", "-l" },
            enable_wayland = true,
          }
        '';
    };
  };
}

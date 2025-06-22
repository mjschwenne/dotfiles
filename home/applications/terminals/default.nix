{
  pkgs,
  osConfig,
  ...
} @ inputs: {
  programs = {
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
        colors.alpha = 0.9;
        colors = {
          foreground = "d8dee9";
          background = "2e3440";

          # selection-foreground = d8dee9
          # selection-background = 4c566a

          regular0 = "3b4252";
          regular1 = "bf616a";
          regular2 = "a3be8c";
          regular3 = "ebcb8b";
          regular4 = "81a1c1";
          regular5 = "b48ead";
          regular6 = "88c0d0";
          regular7 = "e5e9f0";

          bright0 = "4c566a";
          bright1 = "bf616a";
          bright2 = "a3be8c";
          bright3 = "ebcb8b";
          bright4 = "81a1c1";
          bright5 = "b48ead";
          bright6 = "8fbcbb";
          bright7 = "eceff4";

          dim0 = "373e4d";
          dim1 = "94545d";
          dim2 = "809575";
          dim3 = "b29e75";
          dim4 = "68809a";
          dim5 = "8c738c";
          dim6 = "6d96a5";
          dim7 = "aeb3bb";
        };
        cursor = {
          style = "block";
          color = "2e3440 d8dee9";
        };
      };
    };

    kitty = {
      enable = true;
      themeFile = "Nord";
      font.name = "JetBrainsMono Nerd Font";
      shellIntegration = {
        enableFishIntegration = true;
        mode = "no-cursor";
      };
      settings = {
        disable_ligatures = "never";
        cursor_shape = "block";
        share_connections = "no";
        background_opacity = 0.9;
      };
    };

    wezterm = {
      enable = true;
      package = inputs.wezterm.packages.${pkgs.system}.default;
      extraConfig = let
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
          "sol" =
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
        };
      in
        /*
        lua
        */
        ''
          local wezterm = require 'wezterm'
          return {
            front_end = "OpenGL",
            ${preferred_adapter."${osConfig.networking.hostName}"}
            font = wezterm.font("JetBrainsMono Nerd Font", {weight = "Light"}),
            font_size = 11.0,
            color_scheme = "nord",
            window_background_opacity = 0.9,
            hide_tab_bar_if_only_one_tab = true,
            default_prog = { "fish", "-l" },
            enable_wayland = true,
            check_for_updates = false,
            -- show_update_window = false,
          }
        '';
    };
  };
}

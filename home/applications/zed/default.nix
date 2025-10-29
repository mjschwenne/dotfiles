{...}: {
  programs.zed-editor = {
    enable = true;

    extensions = ["nix" "make" "fish"];
    userSettings = {
      auto_update = false;
      telemetry.metrics = false;
      agent = {
        default_open_ai_model = null;
        default_model = {
          provider = "zed.dev";
          model = "claude-sonnet-4-5";
        };
      };
      lsp = {
        nix = {
          binary = {
            path_lookup = true;
          };
        };
      };
      terminal = {
        alternate_scroll = "off";
        blinking = "off";
        copy_on_select = false;
        dock = "bottom";
        detect_venv = {
          on = {
            directories = [".env" "env" ".venv" "venv"];
            activate_script = "default";
          };
        };
        env = {
          TERM = "wezterm";
        };
        font_family = "JetBrainsMono Nerd Font";
        font_features = null;
        font_size = null;
        line_height = "comfortable";
        option_as_meta = false;
        button = false;
        toolbar = {
          title = true;
        };
        working_directory = "current_project_directory";
      };
      hour_format = "hour24";
      vim_mode = true;
      load_direnv = "shell_hook";
      base_keymap = "VSCode";
      show_whitespaces = "all";
    };
  };

  stylix.targets.zed.enable = true;
}

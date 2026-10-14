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
        colors-dark = {
          blur = "on";
        };
        scrollback = {
          lines = 10000;
        };
        cursor.style = "block";
      };
    };

    ghostty = {
      enable = true;
      enableFishIntegration = true;
      systemd.enable = true;
      settings = {
        cursor-style = "block";
        cursor-style-blink = false;
        shell-integration-features = "no-cursor,ssh-env";
        background-opacity-cells = true;
        background-blur = true;
        window-decoration = "auto";
        auto-update = "off";
        notify-on-command-finish = "unfocused";
        notify-on-command-finish-action = "no-bell,notify";
        notify-on-command-finish-after = "60s";
        keybind = [
          "ctrl+enter=unbind"
        ];
      };
    };
  };
}

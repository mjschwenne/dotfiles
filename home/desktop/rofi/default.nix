{pkgs, ...}: {
  programs.rofi = {
    enable = true;
    package = pkgs.rofi;
    extraConfig = {
      modi = "run,drun,window";
      show-icons = true;
      terminal = "kitty";
      drun-display-format = "{icon} {name}";
      disable-history = false;
      hide-scrollbar = true;
      display-drun = "   Apps ";
      display-run = "   Run ";
      display-window = " 󰕰  Window";
      display-Network = " 󰤨  Network";
      sidebar-mode = true;
    };

    theme = {
      "*".width = 600;

      element.padding = 5;

      element-icon.size = 25;

      mode-switcher.spacing = 0;

      window = {
        height = 360;
        border = 3;
        border-radius = 5;
      };

      inputbar = {
        border-radius = 5;
        padding = 2;
      };

      prompt = {
        padding = 6;
        border-radius = 3;
        margin = "20px 0px 0px 20px";
      };

      textbox-prompt-colon = {
        expand = false;
        str = ":";
      };

      entry = {
        padding = 6;
        margin = "20px 0px 0px 20px";
      };

      listview = {
        border = "0px 0px 0px";
        padding = "6px 0px 0px";
        margin = "10px 0px 0px 20px";
        columns = 2;
        lines = 5;
      };

      button = {
        padding = 10;
        vertical-align = "0.5";
        horizontal-align = "0.5";
      };

      message = {
        margin = 2;
        padding = 2;
        border-radius = 5;
      };

      textbox = {
        padding = 6;
        margin = "20px 0px 0px 20px";
      };
    };
  };
}

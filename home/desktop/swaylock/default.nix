{...}: {
  programs.swaylock = {
    enable = true;
    settings = {
      image = "~/.config/swaylock/swaylock.png";
      daemonize = true;
      indicator-radius = 150;
      indicator-thickness = 15;
      indicator-caps-lock = true;

      # font = "JetBrainsMono Nerd Font";
      font-size = 32;
    };
  };

  xdg.configFile = {
    "swaylock/swaylock.png".source = ./swaylock.png;
  };
}

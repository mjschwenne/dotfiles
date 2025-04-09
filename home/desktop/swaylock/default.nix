{...}: {
  programs.swaylock = {
    enable = true;
    settings = {
      image = "~/.config/swaylock/swaylock.png";
      daemonize = true;
      indicator-radius = 150;
      indicator-thickness = 15;
      indicator-caps-lock = true;
      color = "2e344020";
      separator-color = "2e344020";
      key-hl-color = "d08770";
      bs-hl-color = "d08770";

      font = "JetBrainsMono Nerd Font";
      font-size = 32;
      text-caps-lock-color = "ebcb8b";
      text-clear-color = "d8dee9";
      text-ver-color = "d8dee9";
      text-wrong-color = "bf616a";

      inside-clear-color = "4c566a80";
      inside-color = "4c566a80";
      inside-ver-color = "4c566a80";
      inside-wrong-color = "4c566a80";

      line-clear-color = "2e3440";
      line-color = "2e3440";
      line-ver-color = "2e3440";
      line-wrong-color = "2e3440";
      line-caps-lock-color = "81a1c1";

      ring-clear-color = "ebcb8b";
      ring-color = "d8dee9";
      ring-ver-color = "a3be8c";
      ring-wrong-color = "bf616a";
      ring-caps-lock-color = "ebcb8b";
    };
  };

  xdg.configFile = {
    "swaylock/swaylock.png".source = ./swaylock.png;
  };
}

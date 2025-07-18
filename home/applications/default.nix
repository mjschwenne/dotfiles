{...}: {
  imports = [./discord ./librewolf ./spotify ./terminals ./zathura ./nethack];

  programs.mpv = {
    enable = true;
  };
}

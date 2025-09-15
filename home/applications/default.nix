{...}: {
  imports = [./discord ./librewolf ./spotify ./terminals ./zed ./zathura ./nethack];

  programs.mpv = {
    enable = true;
  };
}

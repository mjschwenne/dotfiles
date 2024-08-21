{...}: {
  imports = [./discord ./spotify ./terminals ./zathura];

  programs.mpv = {
    enable = true;
  };
}

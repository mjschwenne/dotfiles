{...}: {
  imports = [./discord ./librewolf ./spotify ./terminals ./zathura ./supernote ./nethack];

  programs.mpv = {
    enable = true;
  };
}

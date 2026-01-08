{...}: {
  imports = [./discord ./librewolf ./spotify ./supernote ./terminals ./zed ./zathura ./nethack];

  programs.mpv = {
    enable = true;
  };
}

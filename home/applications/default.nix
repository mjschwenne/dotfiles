{...}: {
  imports = [./discord ./spotify ./terminals ./zathura ./supernote];

  programs.mpv = {
    enable = true;
  };
}

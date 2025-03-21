{...}: {
  imports = [./discord ./librewolf ./spotify ./terminals ./zathura ./supernote];

  programs.mpv = {
    enable = true;
  };
}

{pkgs, ...}: {
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-pipewire-audio-capture
      obs-backgroundremoval
    ];
  };

  home.file.".config/obs-studio/themes" = {
    source = ./catppuccin;
    recursive = true;
  };
}

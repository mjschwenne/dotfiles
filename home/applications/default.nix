{ ... }:
{
  imports = [
    ./discord
    ./file-sync
    ./librewolf
    ./spotify
    ./supernote
    ./terminals
    ./zed
    ./zathura
    ./nethack
  ];

  programs.mpv = {
    enable = true;
  };
}

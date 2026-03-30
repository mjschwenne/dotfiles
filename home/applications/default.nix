{ ... }:
{
  imports = [
    ./discord
    ./file-sync
    ./librewolf
    ./spotify
    ./supernote
    ./terminals
    ./zathura
    ./nethack
  ];

  programs.mpv = {
    enable = true;
  };
}

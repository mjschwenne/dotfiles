{pkgs, ...}: {
  home = {
    packages = with pkgs; [nethack];
    file.".nethackrc".text = ''
      OPTIONS=windowtype:curses
      OPTIONS=popup_dialog
      OPTIONS=splash_screen
      OPTIONS=perm_invent
    '';
  };
}

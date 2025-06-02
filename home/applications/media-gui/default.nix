{
  osConfig,
  lib,
  pkgs,
  ...
}: {
  # A simple i3 (for kdeconnect remote input support) config
  # for running librewolf in kiosk mode

  imports = [../terminals ./librewolf.nix];

  home.packages = with pkgs; [dconf];

  xsession.windowManager.i3 = {
    enable = true;
    config = rec {
      modifier = "Mod4";
      bars = [];
      keybindings = let
        mod = modifier;
      in
        lib.mkOptionDefault {
          "${mod}+b" = "exec ${pkgs.librewolf}/bin/librewolf";
          "${mod}+Return" = "exec ${pkgs.kitty}/bin/kitty";
          "${mod}+q" = "kill";
          "${mod}+k" = "exec ${pkgs.kdePackages.kdeconnect-kde}/bin/kdeconnect-app";

          # Utility keys
          "XF86AudioPlay" = "exec playerctl play-pause";
          "XF86AudioNext" = "exec playerctl next";
          "XF86AudioPrev" = "exec playerctl previous";
          "XF86AudioStop" = "exec playerctl stop";

          "XF86AudioRaiseVolume" = "exec amixer -D pulse set Master 5%+";
          "XF86AudioLowerVolume" = "exec amixer -D pulse set Master 5%-";
          "XF86AudioMute" = "exec amixer -D pulse set Master 1+ toggle";
        };
      startup = [
        {command = ''${pkgs.bash}/bin/bash -c "sleep 5; ${pkgs.librewolf}/bin/librewolf --kiosk --new-window https://home.schwennesen.org"'';}
        {command = "${pkgs.xorg.xset}/bin/xset s off";}
        {command = "${pkgs.xorg.xset}/bin/xset -dpms";}
      ];
    };
  };

  gtk = {
    enable = true;
    theme = {
      name = "Nordic";
      package = pkgs.nordic;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-nord;
    };
    font = {name = "JetBrainsMono Nerd Font";};
    gtk3.extraConfig = {gtk-decoration-layout = "appmenu:none";};
  };
  home.pointerCursor = {
    name = "Nordzy-cursors";
    gtk.enable = true;
    package = pkgs.nordzy-cursor-theme;
  };
  home.sessionVariables = {
    GTK_THEME = "Nordic";
  };

  programs.autorandr = {
    enable = true;
    profiles.default.config."HDMI-1".scale = {
      y = 2;
      x = 2;
    };
  };

  services = {
    kdeconnect.enable = true;
  };
}

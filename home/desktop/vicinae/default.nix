{
  pkgs,
  lib,
  vicinae,
  vicinae-extensions,
  ...
}:
{
  imports = [ vicinae.homeManagerModules.default ];

  home.packages = with pkgs; [
    brotab
  ];

  services.vicinae = {
    enable = true;
    systemd = {
      enable = true;
      autoStart = true;
      environment = {
        USE_LAYER_SHELL = 1;
      };
    };
    settings = {
      close_on_focus_loss = true;
      pop_to_root_on_close = true;
      launcher_window.opacity = lib.mkForce 0.9;
      providers = {
        "@sovereign/vicinae-extension-awww-switcher-0" = {
          preferences = {
            wallpaperPath = "/home/mjs/.dotfiles/home/desktop/wallpapers";
          };
        };
        "@brpaz/vicinae-extension-brotab-0" = {
          preferences = {
            brotabPath = "${pkgs.brotab}/bin/brotab";
          };
        };
      };
    };
    extensions = with vicinae-extensions.packages.${pkgs.stdenv.hostPlatform.system}; [
      awww-switcher
      bluetooth
      brotab
      niri
      nix
      power-profile
      process-manager
      pulseaudio
      ssh
    ];
  };

  stylix.targets.vicinae.enable = true;
}

{ pkgs, stasis, ... }:
{
  home.packages = with pkgs; [
    pulseaudioFull
  ];

  # imports = [ stasis.homeModules.default ];
  imports = [ ./stasis-module.nix ];
  services.stasis = {
    enable = true;
    extraConfig = ''
      @author "Matt Schwennesen"
      @description "Stasis configuration file"

      default:
        pre_suspend_command "${pkgs.swaylock}/bin/swaylock"
        monitor_media true
        ignore_remote_media false
        respect_idle_inhibitors true
        
        # Optional: ignore specific media players
        #media_blacklist ["spotify"]

        # Applications that prevent idle when active
        #inhibit_apps [
        #  "vlc"
        #  "Spotify"
        #  "mpv"
        #  r".*\.exe"
        #  r"steam_app_.*"
        #  r"firefox.*"
        #]

        lock_screen:
          timeout 300 # 5 minutes 
          command "${pkgs.swaylock}/bin/swaylock"
          resume-command "${pkgs.libnotify}/bin/notify-send 'Welcome back $env.USER'"
        end

        suspend:
          timeout 60  # 1 minute after locking
          command "${pkgs.systemd}/bin/systemctl suspend"
        end
      end
    '';
  };
}

{ pkgs, stasis, ... }:
{
  home.packages = with pkgs; [
    pulseaudioFull
  ];

  imports = [ stasis.homeModules.default ];
  services.stasis = {
    enable = true;
    extraConfig = ''
      @author "Matt Schwennesen"
      @description "Stasis configuration file"

      default:
        # pre_suspend_command "swaylock"
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
          use_loginctl false
          command "swaylock"
          resume-command "notify-send 'Welcome back $env.USER'"
        end

        suspend:
          timeout 600  # 10 minutes
          command "systemctl suspend"
        end
      end
    '';
  };
}

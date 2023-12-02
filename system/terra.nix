{ config, ... }: {
  imports = [
    # Include the results of the hardware scan.
    ./terra-hardware.nix
    ./common.nix
    ./graphical.nix
  ];

  # Bootloader.
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
  '';
  security.polkit.enable = true;

  networking.hostName = "terra"; # Define your hostname.

  programs.steam.enable = true;

  services.syncthing = {
    enable = true;
    user = "mjs";
    configDir = "/home/mjs/.syncthing";
    dataDir = "/home/mjs/.syncthing";

    settings = {
      options.globalAnnounceServer = [ "https://discovery.schwennesen.org" ];
      options.urAccepted = 3;

      devices = {
        "sol" = {
          id = "73R7LD7-CE75DOS-QYCX3IN-7MYT33V-W65EKXH-L5Y2HIU-UGUPESG-KZEUYAF";
        };
        "luna" = {
          id = "43RQHNP-QWSOVCU-32G6M5U-4TSIRY6-Y26QJBR-FT4DNK3-QRIJEAC-TIXHGA4";
        };
        "phone" = {
          id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
        };
      };

      folders = {
        "org" = {
          path = "/home/mjs/Documents";
          devices = [ "sol" "luna" ];
        };
        "kdb" = {
          path = "/home/mjs/kdb";
          devices = [ "sol" "luna" "phone" ];
        };
      };
    };
  };
}

{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./mars-hardware.nix
    ./common.nix
    ./graphical.nix
  ];

  # Bootloader.
  boot.extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
  boot.kernelModules = ["v4l2loopback"];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
  '';
  security.polkit.enable = true;

  sops.secrets = {
    "ssh/mars/github/key".owner = "mjs";
    "ssh/mars/sol/key".owner = "mjs";
  };

  networking.hostName = "mars"; # Define your hostname.

  programs.steam.enable = true;
  programs.kdeconnect.enable = true;

  # android stuff for supernote
  programs.adb.enable = true;
  users.users.mjs.extraGroups = ["adbusers"];
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  services.syncthing = {
    enable = true;
    user = "mjs";
    configDir = "/home/mjs/.syncthing";
    dataDir = "/home/mjs/.syncthing";

    settings = {
      options.globalAnnounceServer = ["https://discovery.schwennesen.org"];
      options.urAccepted = 3;

      devices = {
        "sol" = {
          id = "7AYNHZQ-VFBBFZP-MC327GI-UTDLN4K-KZOV2L6-DVI5Z6D-TORIX5C-IXDYEAP";
        };
        "terra" = {
          id = "C276DRG-ZBCYDM7-JZEA3FC-5F76M2K-O6UU6XF-5TUI4EK-LG437FA-E6RIFQC";
        };
        "luna" = {
          id = "V2BAQL2-R6RK74M-OLBQEY2-MVA2C7B-JX6XLK3-EOSFXLH-2VY4NXR-C5Z33QH";
        };
        "mercury" = {
          id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
        };
        "enceladus" = {
          id = "DVP6EYQ-22EPU7T-A7UYHCB-5H7SQKS-5Q32KE7-NCZPQAC-TAFXCWS-TDOOHAW";
        };
      };

      folders = {
        "org" = {
          path = "/home/mjs/Documents";
          devices = ["sol" "luna" "terra"];
        };
        "kdb" = {
          path = "/home/mjs/kdb";
          devices = ["sol" "luna" "mercury" "terra" "enceladus"];
        };
        "emulator" = {
          path = "/home/mjs/workspace/emulation";
          devices = ["sol" "luna" "terra" "mercury"];
        };
      };
    };
  };
}

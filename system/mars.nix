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
          id = "43RQHNP-QWSOVCU-32G6M5U-4TSIRY6-Y26QJBR-FT4DNK3-QRIJEAC-TIXHGA4";
        };
        "phone" = {
          id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
        };
      };

      folders = {
        "org" = {
          path = "/home/mjs/Documents";
          devices = ["sol" "luna" "terra"];
        };
        "zotero" = {
          path = "/home/mjs/Zotero/storage";
          devices = ["sol" "luna" "terra"];
        };
        "kdb" = {
          path = "/home/mjs/kdb";
          devices = ["sol" "luna" "phone" "terra"];
        };
        "emulator" = {
          path = "/home/mjs/workspace/emulation";
          devices = ["sol" "luna" "terra" "phone"];
        };
      };
    };
  };
}

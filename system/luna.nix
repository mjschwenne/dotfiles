{
  pkgs,
  lib,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./luna-hardware.nix
    ./common.nix
    ./graphical.nix
  ];

  networking.hostName = "luna"; # Define your hostname.

  users.users.mjs.extraGroups = ["surface-control"];
  services.iptsd = {
    enable = true;
    config.Touch.DisableOnStylus = true;
  };

  environment.systemPackages = with pkgs; [
    iptsd
  ];

  sops.secrets = {
    "ssh/luna/github/key".owner = "mjs";
    "ssh/luna/sol/key".owner = "mjs";
  };

  # Enable thermald, which should prevent overheating
  services.thermald.enable = true;

  services.syncthing = {
    enable = true;
    user = "mjs";
    dataDir = "/home/mjs/syncthing";

    settings.options.urAccepted = 3;

    settings = {
      devices = {
        "terra" = {
          id = "C276DRG-ZBCYDM7-JZEA3FC-5F76M2K-O6UU6XF-5TUI4EK-LG437FA-E6RIFQC";
        };
        "mars" = {
          id = "5APBYBW-OQQQUSK-C6LV2SD-B6DZLJL-WKAO532-YBCFNQH-3GHTPT2-47S2WAX";
        };
        "sol" = {
          id = "7AYNHZQ-VFBBFZP-MC327GI-UTDLN4K-KZOV2L6-DVI5Z6D-TORIX5C-IXDYEAP";
        };
        "mercury" = {
          id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
        };
      };

      folders = {
        "org" = {
          path = "/home/mjs/Documents";
          devices = ["terra" "sol" "mars"];
        };
        "zotero" = {
          path = "/home/mjs/Zotero/storage";
          devices = ["sol" "terra" "mars"];
        };
        "kdb" = {
          path = "/home/mjs/kdb";
          devices = ["terra" "sol" "mars" "mercury"];
        };
        "emulator" = {
          path = "/home/mjs/workspace/emulation";
          devices = ["sol" "mars" "terra" "mercury"];
        };
      };
    };
  };
}

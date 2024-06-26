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
          id = "MUIVJMG-P5ZY5S6-AFI7F7F-LQHCRJE-CZEXTWR-F6TEYUP-QCDPQON-VVCIGA2";
        };
        "mars" = {
          id = "SA2KNKU-LTH27QL-OHZSGVW-HCMWWFP-5RYZEER-K3LQKDM-NGF2PNQ-HRVKSQL";
        };
        "sol" = {
          id = "7AYNHZQ-VFBBFZP-MC327GI-UTDLN4K-KZOV2L6-DVI5Z6D-TORIX5C-IXDYEAP";
        };
        "phone" = {
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
          devices = ["terra" "sol" "mars"];
        };
        "emulator" = {
          path = "/home/mjs/workspace/emulation";
          devices = ["sol" "mars" "terra" "phone"];
        };
      };
    };
  };
}

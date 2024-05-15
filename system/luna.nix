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
  microsoft-surface.ipts.enable = lib.mkForce true;

  environment.systemPackages = with pkgs; [
    iptsd
  ];

  sops.secrets = {
    "ssh/luna/github/key".owner = "mjs";
    "ssh/luna/sol/key".owner = "mjs";
  };

  # Adjust screen size for SDDM
  services = {
    xserver.displayManager.sddm.sugarCandyNix.settings = {
      ScreenWidth = lib.mkDefault 2736;
      ScreenHeight = lib.mkDefault 1824;
    };
    # Enable thermald, which should prevent overheating
    thermald.enable = true;
  };

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
        "enumlator" = {
          path = "/home/mjs/workspace/enumlation";
          devices = ["sol" "mars" "terra" "phone"];
        };
      };
    };
  };
}

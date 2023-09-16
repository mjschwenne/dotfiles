{ pkgs, lib, ... }: {
  imports = [
    # Include the results of the hardware scan.
    ./luna-hardware.nix
    ./common.nix
    ./graphical.nix
  ];

  networking.hostName = "luna"; # Define your hostname.

  users.users.mjs.extraGroups = [ "surface-control" ];
  microsoft-surface.ipts.enable = lib.mkForce true;

  environment.systemPackages = with pkgs; [
    iptsd
  ];

  # Adjust screen size for SDDM
  services.xserver.displayManager.sddm.sugarCandyNix.settings = {
    ScreenWidth = lib.mkDefault 2736;
    ScreenHeight = lib.mkDefault 1824;
  };

  services.syncthing = {
    enable = true;
    user = "mjs";
    dataDir = "/home/mjs/syncthing";

    settings = {
      devices = {
        "terra" = {
          id = "SA2KNKU-LTH27QL-OHZSGVW-HCMWWFP-5RYZEER-K3LQKDM-NGF2PNQ-HRVKSQL";
        };
        "sol" = {
          id = "73R7LD7-CE75DOS-QYCX3IN-7MYT33V-W65EKXH-L5Y2HIU-UGUPESG-KZEUYAF";
        };
      };

      folders = {
        "org" = {
          path = "/home/mjs/Documents";
          devices = [ "terra" "sol" ];
        };

        "kdb" = {
          path = "/home/mjs/kdb";
          devices = [ "terra" "sol" ];
        };
      };
    };
  };
}

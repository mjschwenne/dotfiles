{
  config,
  pkgs,
  nixpkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./venus-hardware.nix
    ./common.nix
    ./graphical.nix
  ];

  boot = {
    extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
    kernelModules = ["v4l2loopback"];
    extraModprobeConfig = ''
      options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
    '';
  };

  security.polkit.enable = true;

  networking.hostName = "venus";

  sops.secrets = {
    "venus/ssh/key".owner = "mjs";
    "venus/sol/key".owner = "mjs";
    "venus/tailscale".owner = "mjs";
  };

  nixpkgs.config.allowBroken = true;
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = [
      pkgs.vulkan-validation-layers
    ];
  };

  # tailscale
  services.tailscale.authKeyFile = config.sops.secrets."venus/tailscale".path;

  hardware.keyboard.zsa.enable = true;

  # android stuff for supernote
  programs.adb.enable = true;
  users.users.mjs.extraGroups = ["adbusers"];
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  # fingerprint reader
  services.fprintd = {
    enable = true;
  };
  systemd.services.fprintd = {
    wantedBy = ["multi-user.target"];
    serviceConfig.Type = "simple";
  };

  services.kmonad = {
    enable = true;
    keyboards = {
      laptop = {
        device = "/dev/input/event0";
        config = builtins.readFile ./applications/kmonad/venus-laptop.kdb;
      };
    };
  };
  security.pam.services = {
    greetd.fprintAuth = false;
    login.fprintAuth = true;
    swaylock.fprintAuth = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}

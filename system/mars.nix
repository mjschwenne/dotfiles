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
    "ssh/mars/ssh/key".owner = "mjs";
    "ssh/mars/sol/key".owner = "mjs";
    "nextdns/config".owner = "root";
  };

  networking.hostName = "mars"; # Define your hostname.
  networking.nameservers = ["127.0.0.1" "::1"];
  services.nextdns = {
    enable = true;
    arguments = [
      "-config-file"
      ''${config.sops.secrets."nextdns/config".path}''
    ];
  };

  programs.kdeconnect.enable = true;

  # android stuff for supernote
  programs.adb.enable = true;
  users.users.mjs.extraGroups = ["adbusers"];
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  hardware.keyboard.zsa.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

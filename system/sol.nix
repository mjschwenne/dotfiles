{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./sol-hardware.nix
    ./common.nix
    ./applications/mediaserver
  ];

  networking.hostName = "sol"; # define machine hostname

  programs.dconf.enable = true;
  hardware.bluetooth.enable = true;

  sops.secrets = {
    "sol/ssh/key".owner = "mjs";
    "sol/nix-serve/key".owner = "mjs";
    "sol/tailscale".owner = "mjs";
  };

  services = {
    nix-serve.secretKeyFile = config.sops.secrets."sol/nix-serve/key".path;
    # Disable suspend when laptop lid is closed
    logind.lidSwitch = "ignore";
    tailscale = {
      authKeyFile = config.sops.secrets."sol/tailscale".path;
      extraUpFlags = ["--ssh" "--exit-node-allow-lan-access"];
    };
  };

  # Turn off screen after 60 seconds of inactivity
  boot.kernelParams = ["consoleblank=60"];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

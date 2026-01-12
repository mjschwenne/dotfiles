{
  pkgs,
  config,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./luna-hardware.nix
    ./common.nix
    ./graphical.nix
  ];

  hardware.microsoft-surface.kernelVersion = "stable";
  boot.kernelPatches = [
    {
      name = "rust-1.91-fix";
      patch = ./patches/rust-fix.patch;
    }
  ];

  networking.hostName = "luna"; # Define your hostname.

  users.users.mjs.extraGroups = ["surface-control"];
  services.iptsd = {
    enable = true;
    config.Touchscreen.DisableOnStylus = true;
  };

  environment.systemPackages = with pkgs; [
    iptsd
  ];

  sops.secrets = {
    "luna/ssh/key".owner = "mjs";
    "luna/sol/key".owner = "mjs";
    "luna/tailscale".owner = "mjs";
  };

  services.tailscale.authKeyFile = config.sops.secrets."luna/tailscale".path;
  # Enable thermald, which should prevent overheating
  services.thermald.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

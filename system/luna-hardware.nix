# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "nvme" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
  boot.blacklistedKernelModules = ["ipu3_imgu"];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4a8099f8-c679-4fe8-8c94-15949438e2c1";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/5A1D-CEA8";
    fsType = "vfat";
    options = ["fmask=0022" "dmask=0022"];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/16dacec8-eabb-4e1a-bfbc-10132452608b";
    fsType = "ext4";
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/2d9c69bf-12cd-4a00-9f8b-0b480ce0c3ca";}
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp1s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}

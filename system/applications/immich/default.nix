{...}: {
  services.immich = {
    enable = true;
    port = 2283;
    accelerationDevices = null;
  };

  users.users.immich.extraGroups = ["video" "render"];
}

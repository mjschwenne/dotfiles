{
  config,
  pkgs,
  ...
}: {
  sops.secrets."nextcloud/adminpass" = {
    owner = "nextcloud";
  };
  services = {
    nginx.virtualHosts."localhost".listen = [
      {
        addr = "127.0.0.1";
        port = 8080;
      }
    ];
    nextcloud = {
      enable = true;
      package = pkgs.nextcloud28;
      hostName = "cloud.schwennesen.org";

      # Let NixOS install and configure the database automatically.
      database.createLocally = true;

      # Let NixOS install and configure Redis caching automatically.
      configureRedis = true;

      # Increase the maximum file upload size to avoid problems uploading videos.
      maxUploadSize = "16G";
      https = true;

      autoUpdateApps.enable = true;
      extraAppsEnable = true;
      extraApps = with config.services.nextcloud.package.packages.apps; {
        # List of apps we want to install and are already packaged in
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/servers/nextcloud/packages/nextcloud-apps.json
        inherit calendar contacts notes onlyoffice tasks;

        # Custom app installation example.
        # cookbook = pkgs.fetchNextcloudApp rec {
        #   url = "https://github.com/nextcloud/cookbook/releases/download/v0.10.2/Cookbook-0.10.2.tar.gz";
        #   sha256 = "sha256-XgBwUr26qW6wvqhrnhhhhcN4wkI+eXDHnNSm1HDbP6M=";
        #   license = "agpl3";
        # };
      };

      settings.overwriteprotocol = "https";

      config = {
        adminuser = "admin";
        adminpassFile = config.sops.secrets."nextcloud/adminpass".path;
      };
    };
    onlyoffice = {
      enable = true;
      hostname = "office.schwennesen.org";
    };
  };
}

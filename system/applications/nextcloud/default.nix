{
  config,
  pkgs,
  ...
}: {
  sops.secrets."nextcloud/adminpass" = {
    owner = "nextcloud";
  };

  services = {
    nginx.virtualHosts."cloud.schwennesen.org".listen = [
      {
        addr = "127.0.0.1";
        port = 19000;
      }
    ];
    nextcloud = {
      enable = true;
      package = pkgs.nextcloud28;
      hostName = "cloud.schwennesen.org";

      https = true;

      # Let NixOS install and configure the database automatically.
      database.createLocally = true;

      # Let NixOS install and configure Redis caching automatically.
      configureRedis = true;

      # Increase the maximum file upload size to avoid problems uploading videos.
      maxUploadSize = "16G";

      phpOptions = {
        "opcache.interned_strings_buffer" = "24";
        "maintenace_window_start" = "1";
        "allow_local_remote_servers" = "true";
      };

      autoUpdateApps = {
        enable = true;
        startAt = "04:00:00";
      };
      extraAppsEnable = true;
      extraApps = with config.services.nextcloud.package.packages.apps; {
        # List of apps we want to install and are already packaged in
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/servers/nextcloud/packages/nextcloud-apps.json
        inherit calendar contacts mail notes onlyoffice tasks;

        # Custom app installation example.
        # cookbook = pkgs.fetchNextcloudApp rec {
        #   url = "https://github.com/nextcloud/cookbook/releases/download/v0.10.2/Cookbook-0.10.2.tar.gz";
        #   sha256 = "sha256-XgBwUr26qW6wvqhrnhhhhcN4wkI+eXDHnNSm1HDbP6M=";
        #   license = "agpl3";
        # };
      };

      settings = {
        trusted_proxies = ["192.168.0.206" "127.0.0.1"];
        overwriteprotocol = "https";
        default_phone_region = "US";
        mail_smtpmode = "smtp";
        mail_smtphost = "127.0.0.1:1025";
        mail_smtpsecure = "";
        mail_smtpstreamoptions = {
          ssl = {
            allow_self_signed = true;
            verify_peer = false;
            verify_peer_name = false;
          };
        };
      };

      config = {
        adminuser = "admin";
        adminpassFile = config.sops.secrets."nextcloud/adminpass".path;
        dbtype = "pgsql";
      };
    };
    # onlyoffice = {
    #   enable = true;
    #   hostname = "office.schwennesen.org";
    # };
  };

  # services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "curses";
  };

  environment.systemPackages = with pkgs; [
    gnome.gnome-keyring
    protonmail-bridge
    pass
  ];

  systemd.user.services.protonmail-bridge = {
    enable = true;
    description = "Prontonmail Bridge Server";
    serviceConfig.Type = "simple";
    serviceConfig.ExecStart = ''protonmail-bridge --noninteractive'';
    path = [pkgs.pass];
    wantedBy = ["multi-user.target"];
    after = ["network.target"];
  };
}

{
  config,
  pkgs,
  ...
}: {
  sops.secrets."nextcloud/adminpass" = {
    owner = "nextcloud";
  };

  environment.etc = {
    "fail2ban/filter.d/nextcloud.conf".text = ''
      [Definition]
      _groupsre = (?:(?:,?\s*"\w+":(?:"[^"]+"|\w+))*)
      failregex = ^\{%(_groupsre)s,?\s*"remoteAddr":"<HOST>"%(_groupsre)s,?\s*"message":"Login failed:
                  ^\{%(_groupsre)s,?\s*"remoteAddr":"<HOST>"%(_groupsre)s,?\s*"message":"Trusted domain error.
      datepattern = ,?\s*"time"\s*:\s*"%%Y-%%m-%%d[T ]%%H:%%M:%%S(%%z)?"
    '';
  };

  services = {
    nginx.virtualHosts = {
      "cloud.schwennesen.org" = {
        listen = [
          {
            addr = "127.0.0.1";
            port = 19000;
          }
        ];
        extraConfig = ''
          location ~ \.php(?:$|/) {
            fastcgi_param REQUEST_METHOD $request_method;
            fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
            fastcgi_pass unix:/run/phpfpm/nextcloud.sock;
            fastcgi_buffering on;
            fastcgi_buffers 8 8k;
            fastcgi_busy_buffers_size 16k;
          }
        '';
      };
      "office.schwennesen.org" = {
        listen = [
          {
            addr = "127.0.0.1";
            port = 19001;
          }
        ];
      };
    };

    fail2ban = {
      enable = true;
      maxretry = 5;
      bantime = "24h"; # Ban IPs for one day on the first ban
      bantime-increment = {
        enable = true; # Enable increment of bantime after each violation
        multipliers = "1 2 4 8 16 32 64";
        maxtime = "168h"; # Do not ban for more than 1 week
        overalljails = true; # Calculate the bantime based on all the violations
      };
      jails.nextcloud.settings = {
        enabled = true;
        filter = "nextcloud";
        backend = "auto";
        port = 19000;
      };
    };

    nextcloud = {
      enable = true;
      package = pkgs.nextcloud29;
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
        "cgi.fix_pathinfo" = "1";
        "log_type" = "file";
        "logfile" = "nextcloud.log";
        "loglevel" = 3;
        "logdateformat" = "F d, Y H:i:s";
      };

      autoUpdateApps = {
        enable = true;
        startAt = "04:00:00";
      };

      appstoreEnable = true;
      extraAppsEnable = true;
      extraApps = with config.services.nextcloud.package.packages.apps; {
        # List of apps we want to install and are already packaged in
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/servers/nextcloud/packages/nextcloud-apps.json
        inherit calendar contacts mail notes tasks onlyoffice forms;

        # Custom app installation example.
        # integration_excalidraw = pkgs.fetchNextcloudApp {
        #   url = "https://github.com/nextcloud-releases/integration_excalidraw/releases/download/v2.1.0/integration_excalidraw-v2.1.0.tar.gz";
        #   sha256 = "sha256-ufYw6pVcvHy/ASRuXzsEUCviEVe2kkhsvc75eGDfRFs=";
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
        default_language = "en";
        default_locale = "en_US";
        default_timezone = "America/Chicago";
      };

      config = {
        adminuser = "admin";
        adminpassFile = config.sops.secrets."nextcloud/adminpass".path;
        dbtype = "pgsql";
      };
    };

    onlyoffice = {
      enable = true;
      hostname = "office.schwennesen.org";
    };
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-tty;
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

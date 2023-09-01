{ config, pkgs, lib, ... }:

{
  services.nginx.enable = false;

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud27;
    
    # hosting
    https = true;
    hostName = "nextcloud.schwennesen.org";

    # PHP options 
    phpOptions = {
      upload_max_filesize = lib.mkDefault "8G";
      post_max_size = lib.mkDefault "8G";
    };

    # nextcloud apps 
    extraApps = with config.services.nextcloud.package.packages.apps; {
      inherit news contacts calendar tasks;
    };
    extraAppsEnable = true;

    autoUpdateApps = {
      enable = true;
      startAt = "03:00:00";
    };

    # database config 
    configureRedis = true;
    config = {
      overwriteProtocol = "https";

      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql";
      dbname = "nextcloud";
      dbpassFile = "/var/nextcloud-db-pass";
      
      adminpassFile = "/var/nextcloud-admin-pass";
      adminuser = "admin";
    };
  };

  services.postgresql = {
    enable = true;

    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [
      {
        name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }
    ];
  };

  systemd.services."nextcloud-setup" = {
    requires = [ "postgresql.service" ];
    after = [ "postgresql.service" ];
  };

  services.caddy = {
    enable = true;

    virtualHosts."nextcloud.schwennesen.org".extraConfig = ''
      root * /var/lib/nextcloud
      file_server

      php_fastcgi 127.0.0.1:9000 {
              env PATH /bin
      }

      header {
              # enable HSTS
              Strict-Transport-Security max-age=31536000;
      }

      redir /.well-known/carddav /remote.php/dav 301
      redir /.well-known/caldav /remote.php/dav 301
      @forbidden {
             path /.htaccess
             path /data/*
             path /config/*
             path /db_structure
             path /.xml
             path /README
             path /3rdparty/*
             path /lib/*
             path /templates/*
             path /occ
             path /console.php
      }
      respond @forbidden 404
    '';
  };
}

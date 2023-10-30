{  pkgs
, lib
, foundry
, ...
} : {
  imports = [
    # Include the results of the hardware scan.
    ./sol-hardware.nix
    ./common.nix
    foundry.nixosModules.foundryvtt
  ];

  networking.hostName = "sol"; # define machine hostname

  users.users.mjs.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAGIy0dwGXtG+kNO4OEG3Vv04X7SJHlAYkW7YjtIMnL9 mjs@terra"
  ];

  # Disable suspend when laptop lid is closed
  services.logind.lidSwitch = "ignore";
  boot.kernelParams = [ "consoleblank=60" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    syncthing-discovery
  ];

  services.foundryvtt = {
    enable = true;
    hostName = "schwennesen.org";
    proxySSL = true;
    proxyPort = 443;
  };

  services.caddy = {
    enable = true;

    virtualHosts."foundry.schwennesen.org".extraConfig = ''
      reverse_proxy localhost:30000
    '';

    virtualHosts."syncthing.schwennesen.org".extraConfig = ''
      reverse_proxy localhost:8384
    '';

    virtualHosts."discovery.schwennesen.org".extraConfig = ''
      reverse_proxy localhost:8443 {
        header_up X-Forwarded-For {http.request.remote.host}
        header_up X-Client-Port {http.request.remote.port}
        header_up X-Tls-Client-Cert-Der-Base64 {http.request.tls.client.certificate_der_base64}
      }
      tls {
        client_auth {
          mode request
        }
      }
    '';
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ];
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      X11Forwarding = false; # Since there is no X server over here...
    };
  };

  # Let other hosts find this computer on the network
  services.avahi.enable = true;

  services.syncthing = {
    enable = true;
    user = "mjs";
    configDir = "/home/mjs/.syncthing";

    settings = {
      gui = {
        user = lib.removeSuffix "\n" (builtins.readFile ./syncthing/user);
        password = lib.removeSuffix "\n" (builtins.readFile ./syncthing/password);
        insecureSkipHostcheck = true;
      };

      devices = {
        "terra" = {
          id = "SA2KNKU-LTH27QL-OHZSGVW-HCMWWFP-5RYZEER-K3LQKDM-NGF2PNQ-HRVKSQL";
        };
        "luna" = {
          id = "43RQHNP-QWSOVCU-32G6M5U-4TSIRY6-Y26QJBR-FT4DNK3-QRIJEAC-TIXHGA4";
        };
        "phone" = {
          id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
        };
      };

      folders = {
        "org" = {
          path = "/home/mjs/Documents";
          devices = [ "terra" "luna" ];
        };
        "kdb" = {
          path = "/home/mjs/kdb";
          devices = [ "terra" "luna" "phone" ];
        };
        "agenda" = {
          path = "/home/mjs/Documents/agenda";
          devices = [ "phone" ];
        };
      };
    };
  };

  systemd.services.stdiscosrv = {
    enable = true;
    description = "Syncthing discovery server";
    unitConfig.Type = "simple";
    serviceConfig.ExecStart = ''/run/current-system/sw/bin/stdiscosrv -db-dir="/home/mjs/.syncthing/discovery.db" -http'';
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
  };
}

{
  config,
  pkgs,
  lib,
  foundry,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./sol-hardware.nix
    ./common.nix
    foundry.nixosModules.foundryvtt
    ./applications/nextcloud
  ];

  networking.hostName = "sol"; # define machine hostname

  users.users.mjs.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAGIy0dwGXtG+kNO4OEG3Vv04X7SJHlAYkW7YjtIMnL9 mjs@mars"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE/wOZMUpYq41kO3SSblmEatV/ZScS1QJ0Ez3BkEk78W mjs@terra"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFv/OdgwHQEjIB3/8d8SVfxSP2EHR680ESP3bImiLW3x mjs@luna"
  ];

  nix.sshServe = {
    enable = true;
    keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAGIy0dwGXtG+kNO4OEG3Vv04X7SJHlAYkW7YjtIMnL9 mjs@mars"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE/wOZMUpYq41kO3SSblmEatV/ZScS1QJ0Ez3BkEk78W mjs@terra"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFv/OdgwHQEjIB3/8d8SVfxSP2EHR680ESP3bImiLW3x mjs@luna"
    ];
  };

  sops.secrets = {
    "ssh/sol/ssh/key".owner = "mjs";
    "ssh/sol/nix-serve/key".owner = "mjs";
  };

  services.nix-serve.secretKeyFile = config.sops.secrets."ssh/sol/nix-serve/key".path;

  # Disable suspend when laptop lid is closed
  services.logind.lidSwitch = "ignore";
  boot.kernelParams = ["consoleblank=60"];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    syncthing-discovery
  ];

  services.foundryvtt = {
    enable = true;
    hostName = "schwennesen.org";
    package = foundry.packages.${pkgs.system}.foundryvtt_12;
    proxySSL = true;
    proxyPort = 443;
  };

  services.caddy = {
    enable = true;

    virtualHosts = {
      "cloud.schwennesen.org".extraConfig = ''
        reverse_proxy localhost:19000
      '';

      "foundry.schwennesen.org".extraConfig = ''
        reverse_proxy localhost:30000 {
          header_up Host {host}
          header_up X-Real-IP {remote_host}
          header_up X-Forwarded-For {remote_host}
          header_up X-Forwarded-Proto {scheme}
        }
      '';

      "office.schwennesen.org".extraConfig = ''
        reverse_proxy localhost:8000 {
          header_up X-Forward-Proto https
        }
      '';

      "sync.schwennesen.org".extraConfig = ''
        reverse_proxy localhost:8384
      '';

      "discovery.schwennesen.org".extraConfig = ''
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
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [80 443 8443];
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
    user = "nextcloud";
    configDir = "/var/lib/syncthing/.config";
    dataDir = "/var/lib/syncthing";

    settings = {
      gui = {
        user = lib.removeSuffix "\n" (builtins.readFile ./syncthing/user);
        password = lib.removeSuffix "\n" (builtins.readFile ./syncthing/password);
        insecureSkipHostcheck = true;
      };

      devices = {
        "terra" = {
          id = "C276DRG-ZBCYDM7-JZEA3FC-5F76M2K-O6UU6XF-5TUI4EK-LG437FA-E6RIFQC";
        };
        "mars" = {
          id = "5APBYBW-OQQQUSK-C6LV2SD-B6DZLJL-WKAO532-YBCFNQH-3GHTPT2-47S2WAX";
        };
        "luna" = {
          id = "V2BAQL2-R6RK74M-OLBQEY2-MVA2C7B-JX6XLK3-EOSFXLH-2VY4NXR-C5Z33QH";
        };
        "mercury" = {
          id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
        };
        "enceladus" = {
          id = "DVP6EYQ-22EPU7T-A7UYHCB-5H7SQKS-5Q32KE7-NCZPQAC-TAFXCWS-TDOOHAW";
        };
      };

      folders = {
        "org" = {
          path = "/var/lib/syncthing/documents";
          devices = ["terra" "luna" "mars"];
        };
        "kdb" = {
          path = "/var/lib/syncthing/kdb";
          devices = ["terra" "luna" "mercury" "mars" "enceladus"];
        };
        "emulator" = {
          path = "/var/lib/syncthing/emulation";
          devices = ["terra" "luna" "mars" "mercury"];
        };
      };
    };
  };

  systemd.services.stdiscosrv = {
    enable = true;
    description = "Syncthing discovery server";
    serviceConfig.ExecStart = ''/run/current-system/sw/bin/stdiscosrv --db-dir="/home/mjs/.syncthing/discovery.db" --http --listen=":8443"'';
    wantedBy = ["multi-user.target"];
    after = ["network.target"];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

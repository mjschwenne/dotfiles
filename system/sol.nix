# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config
, pkgs
, lib
, foundry
, ...
} @ inputs: {
  imports = [
    # Include the results of the hardware scan.
    ./sol-hardware.nix
    foundry.nixosModules.foundryvtt
    ./nextcloud.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "sol"; # define machine hostname
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.mjs = {
    isNormalUser = true;
    description = "Matt Schwennesen";
    extraGroups = [ "networkmanager" "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAGIy0dwGXtG+kNO4OEG3Vv04X7SJHlAYkW7YjtIMnL9 mjs@terra"
    ];
    shell = pkgs.fish;
  };

  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Disable suspend when laptop lid is closed
  services.logind.lidSwitch = "ignore";
  boot.kernelParams = [ "consoleblank=60" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
    wget
    curl
    brightnessctl

    syncthing-discovery
  ];

  programs.fish.enable = true;

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

  programs.neovim = {
    enable = true;
    defaultEditor = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

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

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

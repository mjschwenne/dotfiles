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
    "sol/ssh/key".owner = "mjs";
    "sol/nix-serve/key".owner = "mjs";
    "sol/tailscale".owner = "mjs";
  };

  services.nix-serve.secretKeyFile = config.sops.secrets."sol/nix-serve/key".path;

  # Disable suspend when laptop lid is closed
  services.logind.lidSwitch = "ignore";
  boot.kernelParams = ["consoleblank=60"];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  services.tailscale = {
    extraUpFlags = ["--ssh"];
    authKeyFile = config.sops.secrets."sol/tailscale".path;
  };
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
      "foundry.schwennesen.org".extraConfig = ''
        reverse_proxy localhost:30000 {
          header_up Host {host}
          header_up X-Real-IP {remote_host}
          header_up X-Forwarded-For {remote_host}
          header_up X-Forwarded-Proto {scheme}
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

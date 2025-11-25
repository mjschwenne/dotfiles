{
  config,
  pkgs,
  foundry,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./mars-hardware.nix
    ./common.nix
    ./graphical.nix
    ./applications/nextcloud
    ./applications/immich
    foundry.nixosModules.foundryvtt
  ];

  # Bootloader.
  boot = {
    extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
    kernelModules = ["v4l2loopback"];
    extraModprobeConfig = ''
      options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
    '';
  };
  security.polkit.enable = true;

  # Disable suspend when laptop lid is closed
  services.logind.lidSwitch = "ignore";
  boot.kernelParams = ["consoleblank=60"];

  sops.secrets = {
    "mars/ssh/key".owner = "mjs";
    "mars/sol/key".owner = "mjs";
    "mars/tailscale".owner = "mjs";
    "nextdns/config".owner = "root";
    "caddy/envfile".owner = "caddy";
  };

  hardware.keyboard.qmk.enable = true;
  networking.hostName = "mars"; # Define your hostname.
  # networking.nameservers = ["127.0.0.1" "::1"];
  services = {
    tailscale = {
      extraUpFlags = ["--ssh"];
      authKeyFile = config.sops.secrets."mars/tailscale".path;
    };
    foundryvtt = {
      enable = true;
      hostName = "schwennesen.org";
      package = foundry.packages.${pkgs.system}.foundryvtt_13;
      proxySSL = true;
      proxyPort = 443;
    };
    caddy = {
      enable = true;
      package = pkgs.caddy.withPlugins {
        plugins = ["github.com/caddy-dns/porkbun@v0.3.1"];
        hash = "sha256-PUHu+KPywdJMuPLHPtQhUaw3Cv1pED5XQ1MOzlT/6h4=";
      };
      environmentFile = ''${config.sops.secrets."caddy/envfile".path}'';
      extraConfig = ''
        (ts_host) {
            bind {env.TAILNET_IP}

            @blocked not remote_ip 100.64.0.0/10

            tls {
                resolvers 1.1.1.1
                dns porkbun {
                    api_key {env.PORKBUN_API_KEY}
                    api_secret_key {env.PORKBUN_API_PASSWORD}
                }
            }

            respond @blocked "Unauthorized" 403
        }
      '';
      virtualHosts = {
        "cloud.schwennesen.org".extraConfig = ''
          import ts_host
          reverse_proxy localhost:19000
        '';
        "photos.schwennesen.org".extraConfig = ''
          import ts_host
          reverse_proxy localhost:2283
        '';
        # "foundry.schwennesen.org".extraConfig = ''
        #   import ts_host
        #   reverse_proxy localhost:30000 {
        #     header_up Host {host}
        #     header_up X-Real-IP {remote_host}
        #     header_up X-Forwarded-For {remote_host}
        #     header_up X-Forwarded-Proto {scheme}
        #   }
        # '';
      };
    };
    udev.packages = [
      pkgs.android-udev-rules
    ];
  };
  systemd.services.caddy.after = ["mjs-tailscale-up.service"];

  programs.kdeconnect.enable = true;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

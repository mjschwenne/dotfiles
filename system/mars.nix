{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./mars-hardware.nix
    ./common.nix
    ./graphical.nix
    ./applications/nextcloud
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

  networking.hostName = "mars"; # Define your hostname.
  networking.nameservers = ["127.0.0.1" "::1"];
  services = {
    nextdns = {
      enable = true;
      arguments = [
        "-config-file"
        ''${config.sops.secrets."nextdns/config".path}''
      ];
    };
    tailscale = {
      extraUpFlags = ["--ssh"];
      authKeyFile = config.sops.secrets."mars/tailscale".path;
    };
    caddy = {
      enable = true;
      package = pkgs.caddy.withPlugins {
        plugins = ["github.com/caddy-dns/porkbun@v0.2.1"];
        hash = "sha256-X8QbRc2ahW1B5niV8i3sbfpe1OPYoaQ4LwbfeaWvfjg=";
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
      virtualHosts."cloud.schwennesen.org".extraConfig = ''
        import ts_host
        reverse_proxy localhost:19000
      '';
      virtualHosts."office.schwennesen.org".extraConfig = ''
        import ts_host
        reverse_proxy localhost:8000 {
          header_up X-Forward-Proto https
        }
      '';
    };
    udev.packages = [
      pkgs.android-udev-rules
    ];
  };
  systemd.services = {
    mjs-tailscale-up = {
      enable = true;
      after = ["tailscaled.service" "sys-subsystem-net-devices-tailscale0.device"];
      wantedBy = ["multi-user.target"];
      serviceConfig.Type = "oneshot";
      script = ''
        timeout 60s ${pkgs.bash}/bin/bash -c "until ${pkgs.tailscale}/bin/tailscale status --peers=false; do sleep 1; done"
      '';
    };
    caddy = {
      after = ["mjs-tailscale-up.service"];
    };
  };

  programs.kdeconnect.enable = true;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

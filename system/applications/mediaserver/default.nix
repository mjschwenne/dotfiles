{
  config,
  pkgs,
  ...
}: {
  imports = [./gui];
  # Setup the required secrets
  sops.secrets = {
    "homarr/env".owner = "mjs";
    "${config.networking.hostName}/caddy/envfile".owner = "caddy";
  };

  # SERVICES
  services = {
    jellyfin.enable = true;
    sonarr.enable = true;
    caddy = {
      enable = true;
      package = pkgs.caddy.withPlugins {
        plugins = ["github.com/caddy-dns/porkbun@v0.3.1"];
        hash = "sha256-sa+L2YoTM1ZfhfowoCZwmggrUsqw0NmGWRK45TevxFo=";
      };
      environmentFile = config.sops.secrets."${config.networking.hostName}/caddy/envfile".path;
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
        "home.schwennesen.org".extraConfig = ''
          import ts_host
          reverse_proxy localhost:7575
        '';
      };
    };
  };
  systemd.services.caddy.after = ["mjs-tailscale-up.service"];

  # Extra packages
  environment.systemPackages = with pkgs; [
    jellyfin-web
    jellyfin-ffmpeg
  ];

  virtualisation.oci-containers = {
    backend = "podman";
    containers = {
      homarr = {
        image = "ghcr.io/homarr-labs/homarr:latest";
        autoStart = true;
        ports = ["127.0.0.1:7575:7575"];
        environmentFiles = [config.sops.secrets."homarr/env".path];
        volumes = [
          "/home/mjs/homarr/appdata:/appdata"
        ];
      };
    };
  };

  # HARDWARE TRANSCODING
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override {enableHybridCodec = true;};
  };
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      vaapiVdpau
      libvdpau-va-gl
      intel-compute-runtime
      vpl-gpu-rt
      intel-media-sdk
    ];
  };
}

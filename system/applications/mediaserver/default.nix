{
  config,
  pkgs,
  ...
}: {
  # Setup the required secrets
  sops.secrets = {
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
        hash = "sha256-yJyTf2VFh0FKgzIIPxNAlSz0t/lm6dQmRl04823Mij4=";
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

  services.xserver.enable = true;
  services.xserver.desktopManager.kodi = {
    enable = true;
    package = pkgs.kodi.withPackages (kodiPkgs:
      with kodiPkgs; [
        inputstreamhelper
        inputstream-adaptive
        requests
        joystick
      ]);
  };
  services.displayManager.autoLogin.user = "kodi";
  services.xserver.displayManager.lightdm.greeter.enable = false;

  services.joycond.enable = true;

  users.extraUsers.kodi = {
    isNormalUser = true;
    extraGroups = ["data" "video" "audio" "input"];
  };

  programs.kdeconnect.enable = true;

  # Extra packages
  environment.systemPackages = with pkgs; [
    jellyfin-web
    jellyfin-ffmpeg
  ];

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
    ];
  };
}

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
  boot.extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
  boot.kernelModules = ["v4l2loopback"];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
  '';
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
  services.nextdns = {
    enable = true;
    arguments = [
      "-config-file"
      ''${config.sops.secrets."nextdns/config".path}''
    ];
  };
  # tailscale
  services.tailscale = {
    extraUpFlags = ["--ssh"];
    authKeyFile = config.sops.secrets."mars/tailscale".path;
  };
  services.caddy = {
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

  programs.kdeconnect.enable = true;

  # android stuff for supernote
  programs.adb.enable = true;
  users.users.mjs.extraGroups = ["adbusers"];
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  hardware.keyboard.zsa.enable = true;

  services.syncthing = {
    enable = true;
    user = "mjs";
    configDir = "/home/mjs/.syncthing";
    dataDir = "/home/mjs/.syncthing";

    settings = {
      options.globalAnnounceServer = ["https://discovery.schwennesen.org"];
      options.urAccepted = 3;

      devices = {
        "sol" = {
          id = "7AYNHZQ-VFBBFZP-MC327GI-UTDLN4K-KZOV2L6-DVI5Z6D-TORIX5C-IXDYEAP";
        };
        "terra" = {
          id = "C276DRG-ZBCYDM7-JZEA3FC-5F76M2K-O6UU6XF-5TUI4EK-LG437FA-E6RIFQC";
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
          path = "/home/mjs/Documents";
          devices = ["sol" "luna" "terra"];
        };
        "kdb" = {
          path = "/home/mjs/kdb";
          devices = ["sol" "luna" "mercury" "terra" "enceladus"];
        };
        "emulator" = {
          path = "/home/mjs/workspace/emulation";
          devices = ["sol" "luna" "terra" "mercury"];
        };
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

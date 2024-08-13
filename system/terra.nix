{
  config,
  pkgs,
  nixpkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan
    ./terra-hardware.nix
    ./common.nix
    ./graphical.nix
  ];

  boot = {
    extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
    kernelModules = ["v4l2loopback"];
    extraModprobeConfig = ''
      options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
    '';
  };

  security.polkit.enable = true;

  sops.secrets = {
    "ssh/terra/github/key".owner = "mjs";
    "ssh/terra/sol/key".owner = "mjs";
  };

  # limit cores to try and cap memory useage... Which is crazy for a desktop with 64 GB RAM and 64 GB Swap...
  nixpkgs.config.allowBroken = true;
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = [
      pkgs.vulkan-validation-layers
    ];
  };

  services.pipewire.extraConfig.pipewire."90-gpu-output" = {
    "context.modules" = [
      {
        name = "libpipewire-module-combine-stream";
        args = {
          "combine.mode" = "sink";
          "node.name" = "combined_gpu_sink";
          "node.description" = "All GPU Sinks";
          "combine.latency-compensate" = false;
          "combine.props" = {
            "audio.position" = ["FL" "FR"];
          };
          "stream.props" = {};
          "stream.rules" = [
            {
              matches = [
                {
                  "media.class" = "Audio/Sink";
                  "node.name" = "~alsa_output.pci-0000_03_00.1.pro-output.*";
                }
              ];
              actions = {
                create-stream = {};
              };
            }
          ];
        };
      }
    ];
  };
  # Variabled needed to run sway
  environment.variables = {
    WLR_NO_HARDWARE_CURSORS = 1;
  };

  networking.hostName = "terra"; # Define your hostname.

  programs.steam.enable = true;

  services = {
    xserver = {
      wacom.enable = true;
    };

    syncthing = {
      enable = true;
      user = "mjs";
      configDir = "/home/mjs/.syncthing";
      dataDir = "/home/mjs/.syncthing";

      settings = {
        options."globalAnnounceServer" = "https://discovery.schwennesen.org";
        options.urAccepted = 3;

        devices = {
          "sol" = {
            id = "7AYNHZQ-VFBBFZP-MC327GI-UTDLN4K-KZOV2L6-DVI5Z6D-TORIX5C-IXDYEAP";
          };
          "luna" = {
            id = "43RQHNP-QWSOVCU-32G6M5U-4TSIRY6-Y26QJBR-FT4DNK3-QRIJEAC-TIXHGA4";
          };
          "mars" = {
            id = "5APBYBW-OQQQUSK-C6LV2SD-B6DZLJL-WKAO532-YBCFNQH-3GHTPT2-47S2WAX";
          };
          "mercury" = {
            id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
          };
          "enceladus" = {
            id = "YJKHP64-5LEKLBH-OUFD4FP-SPDVVIH-IKQRV7W-FAETK7P-M3H477W-XNZ5PA7";
          };
        };

        folders = {
          "org" = {
            path = "/home/mjs/Documents";
            devices = ["sol" "luna" "mars"];
          };
          "zotero" = {
            path = "/home/mjs/Zotero/storage";
            devices = ["sol" "luna" "mars" "enceladus"];
          };
          "kdb" = {
            path = "/home/mjs/kdb";
            devices = ["sol" "luna" "mercury" "mars" "enceladus"];
          };
          "emulator" = {
            path = "/home/mjs/workspace/emulation";
            devices = ["sol" "luna" "mars" "mercury"];
          };
          "notes" = {
            path = "/home/mjs/notes";
            devices = ["sol" "luna" "mars" "mercury" "enceladus"];
          };
        };
      };
    };
  };
}

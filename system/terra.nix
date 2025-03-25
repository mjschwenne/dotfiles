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
    "ssh/terra/ssh/key".owner = "mjs";
    "ssh/terra/sol/key".owner = "mjs";
  };

  nixpkgs.config.allowBroken = true;
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = [
      pkgs.vulkan-validation-layers
    ];
  };

  services = {
    pipewire.extraConfig.pipewire."90-gpu-output" = {
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
  };
  # Variables needed to run sway
  environment.variables = {
    WLR_NO_HARDWARE_CURSORS = 1;
  };

  networking.hostName = "terra"; # Define your hostname.

  programs.steam.enable = true;

  hardware.keyboard.zsa.enable = true;

  # android stuff for supernote
  programs.adb.enable = true;
  users.users.mjs.extraGroups = ["adbusers"];
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  services = {
    xserver = {
      wacom.enable = true;
    };

    kmonad = {
      enable = true;
      keyboards = {
        kinesisFreestyleEdge = {
          device = "/dev/input/by-id/usb-Kinesis_Freestyle_Edge_RGB_Keyboard_264575131106-if01-event-kbd";
          config = builtins.readFile ./applications/kmonad/freestyle.kdb;
        };
      };
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
            id = "V2BAQL2-R6RK74M-OLBQEY2-MVA2C7B-JX6XLK3-EOSFXLH-2VY4NXR-C5Z33QH";
          };
          "mars" = {
            id = "5APBYBW-OQQQUSK-C6LV2SD-B6DZLJL-WKAO532-YBCFNQH-3GHTPT2-47S2WAX";
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
            devices = ["sol" "luna" "mars"];
          };
          "kdb" = {
            path = "/home/mjs/kdb";
            devices = ["sol" "luna" "mercury" "mars" "enceladus"];
          };
          "emulator" = {
            path = "/home/mjs/workspace/emulation";
            devices = ["sol" "luna" "mars" "mercury"];
          };
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

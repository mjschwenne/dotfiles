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
  # nix.settings.cores = 16;
  # nix.settings.max-jobs = 1;
  nixpkgs.config.allowBroken = true;
  # Nvidia graphics setup
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = [
      pkgs.vulkan-validation-layers
    ];
  };

  # Variabled needed to run sway
  environment.variables = {
    # WLR_DRM_DEVICES = "/dev/dri/card1";
    # WLR_RENDERER = "vulkan";
    # VK_INSTANCE_LAYERS = "VK_LAYER_KHRONOS_validation";
    # GBM_BACKEND = "nvidia-drm";
    # __GL_GSYNC_ALLOWED = 0;
    # __GL_VRR_ALLOWED = 0;
    # WLR_DRM_NO_ATOMIC = 1;
    # QT_AUTO_SCREEN_SCALE_FACTOR = 1;
    # QT_QPA_PLATFORM = "wayland";
    # QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
    # GDK_BACKEND = "wayland";
    # XNT_DESKTOP = "sway";
    # __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    WLR_NO_HARDWARE_CURSORS = 1;
  };

  nixpkgs.config.cudaSupport = false;

  # hardware.nvidia = {
  #   # Modesetting is required.
  #   modesetting.enable = true;
  #
  #   # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
  #   powerManagement.enable = true;
  #   # Fine-grained power management. Turns off GPU when not in use.
  #   # Experimental and only works on modern Nvidia GPUs (Turing or newer).
  #   powerManagement.finegrained = false;
  #
  #   # Use the NVidia open source kernel module (not to be confused with the
  #   # independent third-party "nouveau" open source driver).
  #   # Support is limited to the Turing and later architectures. Full list of
  #   # supported GPUs is at:
  #   # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
  #   # Only available from driver 515.43.04+
  #   # Currently alpha-quality/buggy, so false is currently the recommended setting.
  #   open = false;
  #
  #   # Enable the Nvidia settings menu,
  #   # accessible via `nvidia-settings`.
  #   nvidiaSettings = true;
  #
  #   # Right now 555 seems to be the most stable
  #   package = config.boot.kernelPackages.nvidiaPackages.beta;
  # };

  networking.hostName = "terra"; # Define your hostname.

  programs.steam.enable = true;

  services = {
    xserver = {
      wacom.enable = true;
      # videoDrivers = ["nvidia"];
    };

    syncthing = {
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
          "luna" = {
            id = "43RQHNP-QWSOVCU-32G6M5U-4TSIRY6-Y26QJBR-FT4DNK3-QRIJEAC-TIXHGA4";
          };
          "mars" = {
            id = "SA2KNKU-LTH27QL-OHZSGVW-HCMWWFP-5RYZEER-K3LQKDM-NGF2PNQ-HRVKSQL";
          };
          "phone" = {
            id = "SVMWORW-JCZ26YN-7P77FJC-YYUNZ46-3PXZZQH-TMZGH5F-LD3TVJ4-XEVQMAE";
          };
        };

        folders = {
          "org" = {
            path = "/home/mjs/Documents";
            devices = ["sol" "luna" "mars"];
          };
          "zotero" = {
            path = "/home/mjs/Zotero/storage";
            devices = ["sol" "luna" "mars"];
          };
          "kdb" = {
            path = "/home/mjs/kdb";
            devices = ["sol" "luna" "phone" "mars"];
          };
          "emulator" = {
            path = "/home/mjs/workspace/emulation";
            devices = ["sol" "luna" "mars" "phone"];
          };
        };
      };
    };
  };
}

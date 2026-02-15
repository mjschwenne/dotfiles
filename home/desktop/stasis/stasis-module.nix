{
  config,
  lib,
  pkgs,
  stasis,
  ...
}:

let
  inherit (lib)
    mkIf
    mkEnableOption
    mkPackageOption
    mkOption
    types
    optional
    escapeShellArgs
    getExe
    literalExpression
    makeBinPath
    ;

  package = stasis.packages.${pkgs.stdenv.hostPlatform.system}.stasis;

  cfg = config.services.stasis;

  # IMPORTANT:
  # systemd.user.services.<name>.path expects *packages* (derivations),
  # NOT literal directories. Nix will append /bin automatically.
  servicePathPkgs = with pkgs; [
    bashInteractive
    coreutils
    systemd
  ];

  # Directories to include in PATH for the systemd service.
  defaultServicePath = [
    "/run/current-system/sw/bin"
    "/etc/profiles/per-user/%u/bin"
    "/nix/var/nix/profiles/default/bin"
  ];
in
{
  options.services.stasis = {
    enable = mkEnableOption "Stasis, a lightweight, feature rich Wayland idle manager written in Rust";

    package = mkPackageOption { stasis = package; } "stasis" { };

    extraConfig = mkOption {
      type = types.nullOr types.lines;
      default = null;
      description = ''
        The literal contents of the Stasis configuration file.

        If set, Home Manager will write this text to
        `~/.config/stasis/stasis.rune`.
      '';
      example = literalExpression ''
        # (example omitted)
      '';
    };

    target = mkOption {
      type = types.nonEmptyStr;
      default = config.wayland.systemd.target;
      description = "The systemd user target after which Stasis is started.";
    };

    extraArgs = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra arguments to pass to Stasis.";
    };

    environmentFile = mkOption {
      type = types.nullOr types.str;
      default = "%h/.config/stasis/stasis.env";
      description = ''
        Optional environment file read by the Stasis systemd user service.
        Useful for compositor-specific variables like NIRI_SOCKET.
        Set to null to disable.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    systemd.user.services.stasis = {
      Unit = {
        Description = "Stasis Wayland Idle Manager";
        PartOf = [ cfg.target ];
        After = [ cfg.target ];

        # Restart the service when the config file changes (if HM manages it).
        restartIfChanged = optional (
          cfg.extraConfig != null
        ) config.xdg.configFile."stasis/stasis.rune".source;
      };

      Service = lib.mkMerge [
        (mkIf (cfg.environmentFile != null) {
          EnvironmentFile = [ "-${cfg.environmentFile}" ];
        })
        {
          Type = "simple";
          ExecStart = "${getExe cfg.package} ${escapeShellArgs cfg.extraArgs}";
          Restart = "on-failure";
          Environment = [
            "PATH=${lib.concatStringsSep ":" defaultServicePath}:${makeBinPath servicePathPkgs}"
          ];

          # Only passes vars that exist in the systemd --user manager environment.
          PassEnvironment = [
            "NIRI_SOCKET"
            "WAYLAND_DISPLAY"
            "XDG_RUNTIME_DIR"
          ];
        }
      ];

      # path = servicePathPkgs;

      Install = {
        WantedBy = [ cfg.target ];
      };
    };

    xdg.configFile."stasis/stasis.rune" = mkIf (cfg.extraConfig != null) {
      text = cfg.extraConfig;
    };
  };
}

{
  config,
  lib,
  stasis,
  pkgs,
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

  cfg = config.services.stasis;
  package = stasis.packages.${pkgs.stdenv.hostPlatform.system}.stasis;

  # IMPORTANT:
  # systemd.user.services.<name>.path expects *packages* (derivations),
  # NOT literal directories. Nix will append /bin automatically.
  servicePathPkgs = with pkgs; [
    bashInteractive
    coreutils
    systemd
  ];

  # If you still want the "system profile" bins first, set PATH explicitly.
  # This avoids the /bin/bin bug entirely.
  explicitPath =
    "/run/current-system/sw/bin"
    + ":/etc/profiles/per-user/%u/bin"
    + ":/nix/var/nix/profiles/default/bin"
    + ":${makeBinPath servicePathPkgs}";
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
      };

      # Home Manager supports restartTriggers too; it will still emit
      # X-Restart-Triggers in the unit. If your systemd marks that "bad-setting",
      # you should avoid it and instead rely on `home-manager switch` restart,
      # or add an explicit ExecReload strategy in your app.
      #
      # For now: keep it OFF to avoid the bad-setting state.
      # restartTriggers = optional (cfg.extraConfig != null)
      #   config.xdg.configFile."stasis/stasis.rune".source;

      Service = lib.mkMerge [
        {
          Type = "simple";
          ExecStart = "${getExe cfg.package} ${escapeShellArgs cfg.extraArgs}";
          Restart = "on-failure";

          Slice = "session.slice";

          # Make PATH deterministic and avoid /bin/bin mistakes.
          Environment = [
            "PATH=${explicitPath}"
          ];

          PassEnvironment = [
            "NIRI_SOCKET"
            "WAYLAND_DISPLAY"
            "XDG_RUNTIME_DIR"
            "DBUS_SESSION_BUS_ADDRESS"
          ];
        }
        (mkIf (cfg.environmentFile != null) {
          EnvironmentFile = [ "-${cfg.environmentFile}" ];
        })
      ];

      # IMPORTANT: This is packages, not dirs.
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

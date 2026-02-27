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
    mkMerge
    mkEnableOption
    mkPackageOption
    mkOption
    types
    escapeShellArgs
    getExe
    literalExpression
    ;

  package = stasis.packages.${pkgs.stdenv.hostPlatform.system}.default;
  cfg = config.services.stasis;

  # Base packages available in the service PATH.
  # Include pulseaudio so `pactl` works under PipeWire Pulse.
  baseServicePathPkgs = with pkgs; [
    bashInteractive
    coreutils
    systemd
    pulseaudio
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
        default:
          lock_screen:
            timeout 300
            command "swaylock"
          end
          suspend:
            timeout 600
            command "systemctl suspend"
          end
        end
      '';
    };

    target = mkOption {
      type = types.nonEmptyStr;
      default = config.wayland.systemd.target or "graphical-session.target";
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

    extraPathPackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      example = literalExpression "with pkgs; [ playerctl ]";
      description = ''
        Extra packages added to the Stasis systemd user service PATH.
        (`pulseaudio` is included by default so `pactl` is available.)
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
        ConditionEnvironment = [ "WAYLAND_DISPLAY" ];
      };

      Service = mkMerge [
        {
          Type = "simple";
          ExecStart = "${getExe cfg.package} ${escapeShellArgs cfg.extraArgs}";
          Restart = "on-failure";
          # Slice = "session.slice";

          PassEnvironment = [
            "NIRI_SOCKET"
            "WAYLAND_DISPLAY"
            "XDG_RUNTIME_DIR"
            "DBUS_SESSION_BUS_ADDRESS"
          ];
          Environment = [
            "PATH=${lib.makeBinPath (baseServicePathPkgs ++ cfg.extraPathPackages)}"
          ];
        }
        (mkIf (cfg.environmentFile != null) {
          EnvironmentFile = [ "-${cfg.environmentFile}" ];
        })
      ];
      # systemd will safely construct PATH from derivations.
      # this produces a type error for home manager
      # path = baseServicePathPkgs ++ cfg.extraPathPackages;

      Install = {
        WantedBy = [ cfg.target ];
      };
    };

    xdg.configFile."stasis/stasis.rune" = mkIf (cfg.extraConfig != null) {
      text = cfg.extraConfig;
    };
  };
}

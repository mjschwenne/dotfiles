{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  convertToSeconds =
    timeSpec:
    let
      parts = strings.splitString " " timeSpec;
      totalSeconds = builtins.foldl' (
        acc: part:
        if builtins.match "([0-9]+)(h|m|s)" part != null then
          let
            value = strings.toInt (builtins.substring 0 (builtins.stringLength part - 1) part);
            unit = builtins.substring (builtins.stringLength part - 1) 1 part;
          in
          acc
          + (
            if unit == "h" then
              value * 3600
            else if unit == "m" then
              value * 60
            else if unit == "s" then
              value
            else
              0
          )
        else
          acc
      ) 0 parts;
    in
    builtins.toString totalSeconds;

  cfg = config.services.rclone-sync;

  filterFile = pkgs.writeText "rclone-filters" cfg.filterList;

  mkSyncScript =
    syncConfig:
    pkgs.writeShellScript "rclone-sync-${syncConfig.name}" ''
      LOCAL_DIR="${syncConfig.localPath}"
      REMOTE="${syncConfig.remote}"

      ${pkgs.rclone}/bin/rclone bisync "$LOCAL_DIR" "$REMOTE" \
        --check-access --resilient --filter-from ${filterFile} ${syncConfig.extraArgs}
    '';

  mkRealtimeSyncScript =
    syncConfig:
    pkgs.writeShellScript "rclone-watch-${syncConfig.name}" ''
      WATCH_DIR="${syncConfig.localPath}"
      REMOTE="${syncConfig.remote}"

      do_sync() {
        ${pkgs.rclone}/bin/rclone bisync "$WATCH_DIR" "$REMOTE" \
          --check-access --resilient \
          --filter-from ${filterFile} \
          ${syncConfig.extraArgs}
      }

      # Watch for local changes with timeout for server changes
      while true; do
        if ${pkgs.inotify-tools}/bin/inotifywait -r -t ${convertToSeconds syncConfig.interval} \
             -e modify,create,delete,move "$WATCH_DIR" 2>/dev/null; then
          # Local change detected, debounce
          sleep ${toString syncConfig.debounceSeconds}
        fi
        # Sync on either local change or timeout (server check)
        do_sync
      done
    '';
in
{
  options.services.rclone-sync = {
    enable = mkEnableOption "rclone sync services";

    syncDirs = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              description = "Unique name for this sync configuration";
              example = "classes";
            };

            localPath = mkOption {
              type = types.str;
              description = "Rclone remote path";
              example = "remote:classes";
            };

            remote = mkOption {
              type = types.str;
              description = "Rclone remote path";
              example = "remote:classes";
            };

            mode = mkOption {
              type = types.enum [
                "realtime"
                "timer"
              ];
              default = "timer";
              description = "Sync mode: realtime (inotify) or timer (periodic)";
            };

            interval = mkOption {
              type = types.str;
              default = "1h";
              description = "Sync interval for timer mode (systemd time format using single letter time units)";
              example = "15m";
            };

            debounceSeconds = mkOption {
              type = types.int;
              default = 2;
              description = "Seconds to wait after file changes before syncing (realtime mode)";
            };

            extraArgs = mkOption {
              type = types.str;
              default = "";
              description = "Extra agruments to pass to rclone bisync";
              example = "-v --max-delete 10";
            };
          };
        }
      );
      default = [ ];
      description = "List of directories to sync";
    };

    filterList = mkOption {
      type = types.lines;
      default = "";
      description = "Global filter rules for rclone";
      example = ''
        - .git/ 
        - *.tmp
        - .DS_Store 
        - node_modules/
        + *.txt
      '';
    };

    enableManualSync = mkOption {
      type = types.bool;
      default = true;
      description = "Enable manual sync commands.";
    };
  };

  config = mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        rclone
        inotify-tools
        libnotify
      ]
      # Create simple package for syncing each directory manually
      ++ (optionals cfg.enableManualSync (
        map (
          syncConfig:
          pkgs.writeShellScriptBin "syncnow-${syncConfig.name}" ''
            ${pkgs.rclone}/bin/rclone bisync "${syncConfig.localPath}" "${syncConfig.remote}" \
              --check-access --resilient ${syncConfig.extraArgs} -v --filter-from ${filterFile} $@
            ${pkgs.libnotify}/bin/notify-send "Sync complete" "${syncConfig.name} synced successfully"
          ''
        ) cfg.syncDirs
      ));

    # Create systemd services for each sync directory
    systemd.user.services = listToAttrs (
      map (
        syncConfig:
        nameValuePair "rclone-sync-${syncConfig.name}" {
          Unit = {
            Description = "Rclone sync for ${syncConfig.name}";
            After = [ "network-online.target" ];
          };

          Service =
            if syncConfig.mode == "realtime" then
              {
                Type = "simple";
                ExecStart = "${mkRealtimeSyncScript syncConfig}";
                Restart = "always";
                RestartSec = 10;
              }
            else
              {
                Type = "oneshot";
                ExecStart = "${mkSyncScript syncConfig}";
              };
          Install = mkIf (syncConfig.mode == "realtime") {
            WantedBy = [ "default.target" ];
          };
        }
      ) cfg.syncDirs
    );

    # Create systemd timers for timer-mode syncs
    systemd.user.timers = listToAttrs (
      map (
        syncConfig:
        nameValuePair "rclone-sync-${syncConfig.name}" {
          Unit.Description = "Timer for rclone sync ${syncConfig.name}";

          Timer = {
            OnBootSec = "5min";
            OnUnitActiveSec = syncConfig.interval;
            Persistent = true;
          };

          Install.WantedBy = [ "timers.target" ];
        }
      ) (filter (s: s.mode == "timer") cfg.syncDirs)
    );
  };
}

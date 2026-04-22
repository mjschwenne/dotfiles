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
    toString totalSeconds;

  cfg = config.services.rclone-sync;

  filterFile = pkgs.writeText "rclone-filters" cfg.filterList;

  mkSyncScript =
    syncConfig:
    pkgs.writeShellScript "rclone-sync-${syncConfig.name}" ''
      export PATH="${pkgs.coreutils}/bin:$PATH"
      LOCAL_DIR="${syncConfig.localPath}"
      REMOTE="${syncConfig.remote}"
      LOCKFILE="/tmp/rclone-sync-${syncConfig.name}.lock"

      WAIT_COUNT=0 
      while [ -f "$LOCKFILE" ] && [ $WAIT_COUNT -lt 30 ]; do 
          sleep 1 
          WAIT_COUNT=$((WAIT_COUNT + 1))
      done

      if [ -f "$LOCKFILE" ]; then 
          echo "Another sync is in progress, skipping..." >&2 
          exit 1 
      fi 

      # Create lock file to signal potential realtime watcher
      touch "$LOCKFILE"

      ERRFILE=$(mktemp)
      ${pkgs.rclone}/bin/rclone bisync "$LOCAL_DIR" "$REMOTE" \
        --check-access --resilient --filter-from ${filterFile} ${syncConfig.extraArgs} 2>"$ERRFILE"
      SYNC_EXIT=$?
      if [ $SYNC_EXIT -ne 0 ]; then
        ${optionalString cfg.enableNotifications ''${pkgs.libnotify}/bin/notify-send -u critical "Sync failed" "${syncConfig.name}: $(tail -3 "$ERRFILE")"''}
        cat "$ERRFILE" >&2
      fi
      rm -f "$ERRFILE"

      # Keep lock for a bit to ignore file change events from this sync
      sleep 5
      rm -r "$LOCKFILE"
    '';

  mkManualSyncScript =
    syncConfig:
    pkgs.writeShellScriptBin "syncnow-${syncConfig.name}" ''
      ${pkgs.rclone}/bin/rclone bisync "${syncConfig.localPath}" "${syncConfig.remote}" \
        --check-access --resilient ${syncConfig.extraArgs} -v --filter-from ${filterFile} "$@"
      SYNC_EXIT=$?
      if [ $SYNC_EXIT -ne 0 ]; then
        ${optionalString cfg.enableNotifications ''${pkgs.libnotify}/bin/notify-send -u critical "Sync failed" "${syncConfig.name}"''}
        exit $SYNC_EXIT
      else
        ${optionalString cfg.enableNotifications ''${pkgs.libnotify}/bin/notify-send "Sync complete" "${syncConfig.name} synced successfully"''}
      fi
    '';

  mkFuzzelScript =
    manualSyncScripts: syncDirs:
    pkgs.writeShellApplication {
      name = "rclone-fuzzel";
      runtimeInputs = [ pkgs.fuzzel ] ++ manualSyncScripts;
      text = ''
        placeholder="$(date '+ %a %m-%d  󰥔 %R')"
        choice=$(printf "%s\n" \
          ${concatMapStringsSep " \\\n          " (s: "\"󰑓  ${s.name}\"") syncDirs} \
          "󰑓  sync-all" \
          | fuzzel --dmenu --prompt "󰑓  " --placeholder "         $placeholder")

        case "$choice" in
          ${concatMapStringsSep "\n      " (s: "*\"${s.name}\") syncnow-${s.name} ;;") syncDirs}
          *"sync-all")
            ${concatMapStringsSep "\n        " (s: "syncnow-${s.name} &") syncDirs}
            wait
            ;;
        esac
      '';
    };

  mkRealtimeSyncScript =
    syncConfig:
    pkgs.writeShellScript "rclone-watch-${syncConfig.name}" ''
      export PATH="${pkgs.coreutils}/bin:$PATH"
      WATCH_DIR="${syncConfig.localPath}"
      REMOTE="${syncConfig.remote}"
      LOCKFILE="/tmp/rclone-sync-${syncConfig.name}.lock" 

      do_sync() {
        WAIT_COUNT=0 
        while [ -f "$LOCKFILE" ] && [ $WAIT_COUNT -lt 30 ]; do 
            sleep 1 
            WAIT_COUNT=$((WAIT_COUNT + 1))
        done

        if [ -f "$LOCKFILE" ]; then 
            echo "Another sync is in progress, skipping..." >&2 
            exit 1 
        fi 

        # Create lock file
        touch "$LOCKFILE"

        ERRFILE=$(mktemp)
        ${pkgs.rclone}/bin/rclone bisync "$WATCH_DIR" "$REMOTE" \
          --check-access --resilient \
          --filter-from ${filterFile} \
          ${syncConfig.extraArgs} 2>"$ERRFILE"
        SYNC_EXIT=$?
        if [ $SYNC_EXIT -ne 0 ]; then
          ${optionalString cfg.enableNotifications ''${pkgs.libnotify}/bin/notify-send -u critical "Sync failed" "${syncConfig.name}: $(tail -3 "$ERRFILE")"''}
          cat "$ERRFILE" >&2
        fi
        rm -f "$ERRFILE"

        # Keep lock for a bit to ignore file change events from this sync
        sleep 5
        rm -r "$LOCKFILE"
      }

      do_sync

      # Watch for local changes with timeout for server changes
      while ${pkgs.inotify-tools}/bin/inotifywait -r -e modify,create,delete,move "$WATCH_DIR" 2>/dev/null; do
        sleep ${toString syncConfig.debounceSeconds}
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

            calendar = mkOption {
              type = types.str;
              default = "hourly";
              description = ''
                Calendar specification for timer mode (systemd.time format).
                Examples:
                  "hourly" - every hour
                  "*:0/15" - every 15 minutes
                  "00:00" - daily at midnight
                  "Mon,Wed,Fri 09:00" - specific days
              '';
              example = "*:0/30";
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

    enableNotifications = mkOption {
      type = types.bool;
      default = true;
      description = "Enable desktop notifications for sync events.";
    };
  };

  config = mkIf cfg.enable (
    let
      manualSyncScripts = map mkManualSyncScript cfg.syncDirs;
      fuzzelScript = mkFuzzelScript manualSyncScripts cfg.syncDirs;
    in
    {
    home.packages =
      with pkgs;
      [
        rclone
        inotify-tools
        libnotify
      ]
      ++ (optionals cfg.enableManualSync (manualSyncScripts ++ [ fuzzelScript ]));

    # Create systemd services for each sync directory
    systemd.user.services =
      # Oneshot services for each folder being synced
      listToAttrs (
        map (
          syncConfig:
          nameValuePair "rclone-sync-${syncConfig.name}" {
            Unit = {
              Description = "Rclone sync for ${syncConfig.name}";
              After = [ "network-online.target" ];
            };

            Service = {
              Type = "oneshot";
              ExecStart = "${mkSyncScript syncConfig}";
            };
          }
        ) cfg.syncDirs
      )
      # Long-running watcher for realtime folders
      // listToAttrs (
        map (
          syncConfig:
          nameValuePair "rclone-sync-${syncConfig.name}-watch" {
            Unit = {
              Description = "Rclone realtime sync for ${syncConfig.name}";
              After = [ "network-online.target" ];
            };

            Service = {
              Type = "simple";
              ExecStart = "${mkRealtimeSyncScript syncConfig}";
              Restart = "always";
              RestartSec = 10;
            };

            Install.WantedBy = [ "default.target" ];
          }
        ) (filter (s: s.mode == "realtime") cfg.syncDirs)
      );

    # Create systemd timers for timer-mode syncs
    systemd.user.timers = listToAttrs (
      map (
        syncConfig:
        nameValuePair "rclone-sync-${syncConfig.name}" {
          Unit.Description = "Timer for rclone sync ${syncConfig.name}";

          Timer = {
            Unit = "rclone-sync-${syncConfig.name}.service";
            OnCalendar = if syncConfig.mode == "realtime" then "*:0/5" else syncConfig.calendar;
            Persistent = true;
            RandomizedDelaySec = "1min";
          };

          Install.WantedBy = [ "timers.target" ];
        }
      ) cfg.syncDirs
    );
  });
}

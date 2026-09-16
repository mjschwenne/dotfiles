{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.journal-gen;

  # Bundle the elisp extractor and typst template together so typst's
  # --root points at a real directory rather than the Nix store root.
  journalSrc = pkgs.runCommandLocal "journal-gen-src" { } ''
    mkdir -p $out
    cp ${./journal-extract.el} $out/journal-extract.el
    cp ${./journal.typ}        $out/journal.typ
  '';

  script = pkgs.writeShellApplication {
    name = "journal-gen";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.emacs-nox
      pkgs.typst
    ];
    text = ''
      DATE=$(date +%Y-%m-%d)
      ORG_FILE="${cfg.orgDir}/$DATE.org"
      OUT="${cfg.outputDir}/daily-journal.pdf"
      STAMP="''${STATE_DIRECTORY:-/tmp}/last-run"

      TMP_JSON=$(mktemp /tmp/journal-XXXXXX.json)
      # Named rather than mktemp'd so nothing is created in the copyparty
      # volume until there is actually a PDF to write.
      TMP_PDF="${cfg.outputDir}/.daily-journal.$$.pdf"
      trap 'rm -f "$TMP_JSON" "$TMP_PDF"' EXIT

      # Date pins the stamp to the day, so the first run after midnight always
      # regenerates; mtime+size notices a newer upload landing mid-run. Days
      # with no org file get their own value so they render exactly once.
      fingerprint() {
        if [ -f "$ORG_FILE" ]; then
          printf '%s %s\n' "$DATE" "$(stat -c '%Y %s' "$ORG_FILE")"
        else
          printf '%s blank\n' "$DATE"
        fi
      }

      # Renders TMP_JSON over the output PDF. Publishing is a rename because
      # the Supernote pulls on its own schedule rather than mounting this
      # directory: it only guards a pull landing mid-write, but costs nothing.
      publish() {
        mkdir -p "${cfg.outputDir}"
        typst compile \
          --root / \
          --input "data=$TMP_JSON" \
          ${journalSrc}/journal.typ \
          "$TMP_PDF"
        chmod 644 "$TMP_PDF"
        mv -f "$TMP_PDF" "$OUT"
      }

      # An upload arrives as a burst of inotify events; let it settle so one
      # run picks up the finished file instead of racing the writer.
      sleep ${toString cfg.settleSeconds}

      if [ -f "$STAMP" ] && [ "$(fingerprint)" = "$(cat "$STAMP")" ]; then
        echo "journal-gen: nothing changed since last run, nothing to do"
        exit 0
      fi

      # Weekends and holidays have no org file. That is not an error: write a
      # dated sheet of ruled paper so the Supernote still pulls something
      # usable with the right date on it.
      if [ ! -f "$ORG_FILE" ]; then
        printf '{"date":"%s","blank":true,"planned":[],"plan_text":"","completed":[],"reflection":"","tomorrow":""}\n' \
          "$(date '+%Y-%m-%d %A')" > "$TMP_JSON"
        publish
        fingerprint > "$STAMP"
        echo "journal-gen: no org file for $DATE, wrote blank ruled template"
        exit 0
      fi

      attempt=1
      while :; do
        BEFORE=$(fingerprint)

        emacs --batch \
          -l ${journalSrc}/journal-extract.el \
          --eval "(mjs/je-extract-journal \"$ORG_FILE\")" \
          > "$TMP_JSON"

        publish
        printf '%s\n' "$BEFORE" > "$STAMP"
        echo "journal-gen: wrote $OUT from $ORG_FILE"

        # systemd drops the inotify watches while we run, so an upload that
        # landed during generation would otherwise be lost until tomorrow.
        if [ "$(fingerprint)" = "$BEFORE" ]; then
          break
        fi
        if [ "$attempt" -ge 3 ]; then
          echo "journal-gen: $ORG_FILE still changing after $attempt runs, giving up" >&2
          break
        fi
        echo "journal-gen: $ORG_FILE changed during generation, re-running"
        attempt=$((attempt + 1))
      done
    '';
  };
in
{
  options.services.journal-gen = {
    enable = lib.mkEnableOption "daily journal PDF generation";

    orgDir = lib.mkOption {
      type = lib.types.str;
      default = "/home/mjs/Documents/journal";
      description = "Directory containing YYYY-MM-DD.org journal files.";
    };

    outputDir = lib.mkOption {
      type = lib.types.str;
      description = ''
        Directory to write daily-journal.pdf into.
        The file is overwritten on every run.
        Point this at a copyparty volume root (or a subdirectory of one)
        and ensure the service user has write access.
      '';
      example = "/var/lib/copyparty/data/journal";
    };

    watch = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Regenerate the PDF whenever a file lands in orgDir, rather than only
        on the timer. Uses an inotify path unit, so it fires however the file
        arrives (copyparty upload, rclone sync, a local edit).

        Runs are deduplicated against the org file's mtime and size, so
        unrelated uploads into orgDir cost one stat and exit.
      '';
    };

    settleSeconds = lib.mkOption {
      type = lib.types.ints.unsigned;
      default = 15;
      description = ''
        How long to wait after a change in orgDir before generating, to
        coalesce an upload's burst of events into a single run.
      '';
    };

    calendar = lib.mkOption {
      type = lib.types.str;
      default = "06:00";
      description = ''
        systemd OnCalendar expression controlling when to run.
        With watch enabled this is a backstop for days when no upload
        arrives (or arrives while the machine is off).
      '';
      example = "*-*-* 06:30:00";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "copyparty";
      description = "Unix user to run the service as.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.journal-gen = {
      description = "Generate daily journal PDF from org notes";
      # The path unit can retrigger as fast as uploads land; don't let
      # systemd's default start rate limit wedge the service.
      startLimitIntervalSec = 0;
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        StateDirectory = "journal-gen";
        ExecStart = "${script}/bin/journal-gen";
      };
    };

    systemd.paths.journal-gen = lib.mkIf cfg.watch {
      description = "Watch for journal org files uploaded to the file server";
      wantedBy = [ "multi-user.target" ];
      pathConfig = {
        # PathChanged rather than PathModified: fires on IN_CLOSE_WRITE and
        # IN_MOVED_TO (an upload finishing, or a .PARTIAL being renamed into
        # place) instead of on every write of a streaming upload.
        PathChanged = cfg.orgDir;
      };
    };

    systemd.timers.journal-gen = {
      description = "Daily journal PDF generation timer";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.calendar;
        Persistent = true;
      };
    };
  };
}

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
      pkgs.emacs-nox
      pkgs.typst
    ];
    text = ''
      DATE=$(date +%Y-%m-%d)
      ORG_FILE="${cfg.orgDir}/$DATE.org"
      TMP_JSON=$(mktemp /tmp/journal-XXXXXX.json)
      trap 'rm -f "$TMP_JSON"' EXIT

      if [ ! -f "$ORG_FILE" ]; then
        echo "journal-gen: no org file for today: $ORG_FILE" >&2
        exit 1
      fi

      emacs --batch \
        -l ${journalSrc}/journal-extract.el \
        --eval "(mjs/je-extract-journal \"$ORG_FILE\")" \
        > "$TMP_JSON"

      mkdir -p "${cfg.outputDir}"
      typst compile \
        --root / \
        --input "data=$TMP_JSON" \
        ${journalSrc}/journal.typ \
        "${cfg.outputDir}/daily-journal.pdf"
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

    calendar = lib.mkOption {
      type = lib.types.str;
      default = "06:00";
      description = "systemd OnCalendar expression controlling when to run.";
      example = "*-*-* 06:30:00";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "mjs";
      description = "Unix user to run the service as.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.journal-gen = {
      description = "Generate daily journal PDF from org notes";
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        ExecStart = "${script}/bin/journal-gen";
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

# organice is a frontend-only Org mode client: the build is a directory of
# static files and there is no backend to talk to. This module builds those
# files and points a static file server at them on a loopback port, leaving
# TLS, hostnames and access control to the reverse proxy in front of it.
#
#   services.organice = {
#     enable = true;
#     port = 5000;
#     webdavUrl = "https://files.schwennesen.org/webdav";
#   };
#
#   services.caddy.virtualHosts."org.schwennesen.org".extraConfig = ''
#     import ts_host
#     reverse_proxy localhost:5000
#   '';
#
# Documents live in whichever backend is picked at sign-in (WebDAV, Dropbox,
# GitLab), reached by the browser directly -- never by this server.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.organice;
  webroot = "${cfg.package}/share/organice";

  # static-web-server caches hashed assets for a year but also hands index.html
  # a day of max-age with no validator, which would pin browsers to a stale app
  # shell for a day after every rebuild. parcel hashes every asset filename, so
  # cache the hashed files forever and make everything else revalidate.
  serverConfig = pkgs.writeText "organice-sws.toml" ''
    [advanced]

    [[advanced.headers]]
    source = "/**"
    [advanced.headers.headers]
    Cache-Control = "no-cache"

    [[advanced.headers]]
    source = "/*.*.{js,css,map,png,jpg,svg,ico,woff,woff2,ttf,otf,eot}"
    [advanced.headers.headers]
    Cache-Control = "public, max-age=31536000, immutable"
  '';
in
{
  options.services.organice = {
    enable = lib.mkEnableOption "organice, a browser-based Org mode client";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ./package.nix {
        inherit (cfg)
          dropboxClientId
          gitlabClientId
          gitlabSecret
          webdavUrl
          ;
      };
      defaultText = lib.literalExpression "pkgs.callPackage ./package.nix { }";
      description = ''
        The built organice bundle. Overriding this ignores {option}`webdavUrl`
        and the client id options, since those are baked into the default
        package at build time rather than read at runtime.
      '';
    };

    address = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = ''
        Address to listen on. The default keeps organice off the network, so it
        is reachable only through a reverse proxy on this host.
      '';
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 5000;
      description = "TCP port to serve organice on.";
    };

    webdavUrl = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        Prefills the WebDAV URL on the sign-in screen. This is only a default
        for the form field: the browser talks to that server directly, so it
        has to be reachable from wherever organice is opened, and it has to
        answer `OPTIONS` with permissive CORS headers
        (`Access-Control-Allow-Origin`, `-Methods` including `PROPFIND` and
        `PUT`, `-Headers` including `Authorization` and `Depth`, and
        `-Credentials: true`).
      '';
      example = "https://files.schwennesen.org/webdav";
    };

    dropboxClientId = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        Dropbox app key, enabling the Dropbox sync backend. The app's OAuth
        redirect URI has to list this instance's URL.
      '';
    };

    gitlabClientId = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "GitLab application ID, enabling the GitLab sync backend.";
    };

    gitlabSecret = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        GitLab application secret.

        Despite the name this is not a secret and must not be treated as one:
        organice is frontend only, so the value is compiled into JavaScript
        that every visitor downloads (and, on the way there, into a
        world-readable path in the Nix store). Only use a GitLab application
        that is fine being public.
      '';
    };

    logLevel = lib.mkOption {
      type = lib.types.enum [
        "error"
        "warn"
        "info"
        "debug"
        "trace"
      ];
      default = "error";
      description = ''
        Log level for the static file server. Anything above `error` logs a
        line per request to the journal.
      '';
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra command line arguments for {command}`static-web-server`.";
      example = [
        "--compression"
        "true"
      ];
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.organice = {
      description = "organice, a browser-based Org mode client";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "exec";
        ExecStart = lib.escapeShellArgs (
          [
            (lib.getExe pkgs.static-web-server)
            "--config-file"
            serverConfig
            "--host"
            cfg.address
            "--port"
            (toString cfg.port)
            "--root"
            webroot
            # organice routes client side with BrowserRouter, so a deep link
            # such as /file/notes.org is a real request this server has no file
            # for. Answer those with index.html and a 200 and let React sort
            # the path out once it has booted.
            "--page-fallback"
            "${webroot}/index.html"
            "--log-level"
            cfg.logLevel
          ]
          ++ cfg.extraArgs
        );
        Restart = "on-failure";

        # Nothing to persist and nothing to protect: the app is read-only files
        # in the store, and every document lives in the browser or in the sync
        # backend the browser was pointed at.
        DynamicUser = true;
        AmbientCapabilities = lib.optional (cfg.port < 1024) "CAP_NET_BIND_SERVICE";
        CapabilityBoundingSet = if cfg.port < 1024 then [ "CAP_NET_BIND_SERVICE" ] else [ "" ];
        NoNewPrivileges = true;
        DevicePolicy = "closed";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProtectSystem = "strict";
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
          "~@resources"
        ];
        UMask = "0077";
      };
    };
  };
}

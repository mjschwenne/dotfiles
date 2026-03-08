{ pkgs, config, ... }:
{

  home.packages = with pkgs; [ tridactyl-native ];

  home.file =
    let
      stylix-theme = /* css */ ''
        :root {
            --base00: ${config.lib.stylix.colors.withHashtag.base00};
            --base01: ${config.lib.stylix.colors.withHashtag.base01};
            --base02: ${config.lib.stylix.colors.withHashtag.base02};
            --base03: ${config.lib.stylix.colors.withHashtag.base03};
            --base04: ${config.lib.stylix.colors.withHashtag.base04};
            --base05: ${config.lib.stylix.colors.withHashtag.base05};
            --base06: ${config.lib.stylix.colors.withHashtag.base06};
            --base07: ${config.lib.stylix.colors.withHashtag.base07};
            --base08: ${config.lib.stylix.colors.withHashtag.base08};
            --base09: ${config.lib.stylix.colors.withHashtag.base09};
            --base0A: ${config.lib.stylix.colors.withHashtag.base0A};
            --base0B: ${config.lib.stylix.colors.withHashtag.base0B};
            --base0C: ${config.lib.stylix.colors.withHashtag.base0C};
            --base0D: ${config.lib.stylix.colors.withHashtag.base0D};
            --base0E: ${config.lib.stylix.colors.withHashtag.base0E};
            --base0F: ${config.lib.stylix.colors.withHashtag.base0F};

            --tridactyl-fg: var(--base05);
            --tridactyl-bg: var(--base00);
            --tridactyl-url-fg: var(--base08);
            --tridactyl-url-bg: var(--base00);
            --tridactyl-highlight-box-bg: var(--base0B);
            --tridactyl-highlight-box-fg: var(--base00);

            /* Hint character tags */
            --tridactyl-hintspan-fg: var(--base00) !important;
            --tridactyl-hintspan-bg: var(--base0A) !important;

            /* Element Highlights */
            --tridactyl-hint-active-fg: none;
            --tridactyl-hint-active-bg: none;
            --tridactyl-hint-active-outline: none;
            --tridactyl-hint-bg: none;
            --tridactyl-hint-outline: none;
        }

        #command-line-holder {    order: 1;
            border: 2px solid var(--base0B);
            color: var(--tridactyl-bg);
        }

        #tridactyl-input {    padding: 1rem;
            color: var(--tridactyl-fg);
            width: 90%;
            font-size: 1.5rem;
            line-height: 1.5;
            background: var(--tridactyl-bg);
            padding-left: unset;
            padding: 1rem;
        }

        #completions table {    font-size: 0.8rem;
            font-weight: 200;
            border-spacing: 0;
            table-layout: fixed;
            padding: 1rem;
            padding-top: 1rem;
            padding-bottom: 1rem;
        }

        #completions > div {    max-height: calc(20 * var(--option-height));
            min-height: calc(10 * var(--option-height));
        }

        /* COMPLETIONS */

        #completions {    --option-height: 1.4em;
            color: var(--tridactyl-fg);
            background: var(--tridactyl-bg);
            display: inline-block;
            font-size: unset;
            font-weight: 200;
            overflow: hidden;
            width: 100%;
            border-top: unset;
            order: 2;
        }

        /* Olie doesn't know how CSS inheritance works */
        #completions .HistoryCompletionSource {    max-height: unset;
            min-height: unset;
        }

        #completions .HistoryCompletionSource table {    width: 100%;
            font-size: 9pt;
            border-spacing: 0;
            table-layout: fixed;
        }

        /* redundancy 2: redundancy 2: more redundancy */
        #completions .BmarkCompletionSource {    max-height: unset;
            min-height: unset;
        }

        #completions table tr td.prefix,#completions table tr td.privatewindow,#completions table tr td.container,#completions table tr td.icon {    display: none;
        }

        #completions .BufferCompletionSource table {    width: unset;
            font-size: unset;
            border-spacing: unset;
            table-layout: unset;
        }

        #completions table tr .title {    width: 50%;
        }

        #completions table tr {    white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }

        #completions .sectionHeader {    background: unset;
            font-weight: 200;
            border-bottom: unset;
            padding: 1rem !important;
            padding-left: unset;
            padding-bottom: 0.2rem;
        }

        #cmdline_iframe {    position: fixed !important;
            bottom: unset;
            top: 25% !important;
            left: 10% !important;
            z-index: 2147483647 !important;
            width: 80% !important;
            box-shadow: rgba(0, 0, 0, 0.5) 0px 0px 20px !important;
        }

        .TridactylStatusIndicator {    position: fixed !important;
            bottom: 0 !important;
            background: var(--tridactyl-bg) !important;
            border: unset !important;
            border: 1px var(--base0B) solid !important;
            font-size: 12pt !important;
            /*font-weight: 200 !important;*/
            padding: 0.8ex !important;
        }

        #completions .focused {    background: var(--base0B);
            color: var(--base00);
        }

        #completions .focused .url {    background: var(--base0B);
            color: var(--base00);
        }
        /* #Ocean-normal { */
        /*  border-color: green !important; */
        /* } */

        /* #Ocean-insert { */
        /*  border-color: yellow !important; */
        /* } */
      '';
    in
    {
      ".librewolf/native-messaging-hosts/tridactyl.json".source =
        "${pkgs.tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";
      ".librewolf/default/chrome/userChrome.css".source = ./userChrome.css;
      ".config/tridactyl/tridactylrc".text = ''
        set configversion 2.0
        set browser librewolf
        set theme stylix 
      '';
      ".config/tridactyl/themes/stylix.css".text = stylix-theme;
      ".config/tridactyl/themes/stylix/stylix.css".text = stylix-theme;
    };

  programs.librewolf = {
    enable = true;
    policies = {
      ExtensionSettings = {
        "*".installation_mode = "blocked";

        "addon@simplelogin" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/simplelogin/latest.xpi";
          installation_mode = "force_installed";
        };

        "keepassxc-browser@keepassxc.org" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/keepassxc-browser/latest.xpi";
          installation_mode = "force_installed";
        };

        "zotero@chnm.gmu.edu" = {
          install_url = "https://www.zotero.org/download/connector/dl?browser=firefox&version=5.0.181";
          installation_mode = "force_installed";
        };

        "uBlock0@raymondhill.net" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
          installation_mode = "force_installed";
        };

        "FirefoxColor@mozilla.com" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/firefox-color/latest.xpi";
          installation_mode = "force_installed";
        };

        "tridactyl.vim@cmcaine.co.uk" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/tridactyl-vim/latest.xpi";
          installation_mode = "force_installed";
        };
      };
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
      DisablePocket = true;
      DisableFirefoxAccounts = true;
      # DisableAccounts = true;
      DisableFirefoxScreenshots = true;
      # OverrideFirstRunPage = "";
      # OverridePostUpdatePage = "";
      DontCheckDefaultBrowser = true;
      DisplayBookmarksToolbar = "never"; # alternatives: "always" or "newtab"
      DisplayMenuBar = "never"; # alternatives: "always", "never" or "default-on"
      SearchBar = "unified"; # alternative: "separate"
      SecurityDevices = {
        Add = {
          "OpenSC PKCS#11" = "${pkgs.opensc}/lib/opensc-pkcs11.so";
        };
      };
    };
    profiles.default = {
      extensions.force = true;
      search = {
        force = true;
        engines = {
          # don't need these default ones
          "amazondotcom-us".metaData.hidden = true;
          "bing".metaData.hidden = true;
          "ebay".metaData.hidden = true;
          "ddg" = {
            urls = [
              {
                template = "https://duckduckgo.com";
                params = [
                  {
                    name = "q";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ ",d" ];
          };
          "google" = {
            urls = [
              {
                template = "https://google.com/search";
                params = [
                  {
                    name = "q";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ ",g" ];
          };
          "Home Manager Options" = {
            urls = [
              {
                template = "https://mipmip.github.io/home-manager-option-search/";
                params = [
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "ho" ];
          };
          "Nix Packages" = {
            urls = [
              {
                template = "https://search.nixos.org/packages";
                params = [
                  {
                    name = "type";
                    value = "packages";
                  }
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "np" ];
          };
          "youtube" = {
            urls = [
              {
                template = "https://www.youtube.com/results";
                params = [
                  {
                    name = "search_query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "y" ];
          };
          "Wikipedia" = {
            urls = [
              {
                template = "https://en.wikipedia.org/wiki/Special:Search";
                params = [
                  {
                    name = "search";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "wik" ];
          };
          "GitHub" = {
            urls = [
              {
                template = "https://github.com/search";
                params = [
                  {
                    name = "q";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "gh" ];
          };
        };
        default = "ddg";
      };
      id = 0;
      settings = {
        # settings = lib.mapAttrs' (n: lib.nameValuePair "pref.${n}") {
        "app.update.auto" = true; # disable auto update
        "dom.security.https_only_mode" = true; # force https
        "extensions.pocket.enabled" = false; # disable pocket
        "browser.quitShortcut.disabled" = true; # disable ctrl+q
        "browser.download.panel.shown" = true; # show download panel
        "signon.rememberSignons" = false; # disable saving passwords
        "identity.fxaccounts.enabled" = false; # disable librewolf accounts
        "browser.shell.checkDefaultBrowser" = false; # don't check if default browser
        "browser.bookmarks.restore_default_bookmarks" = false; # don't restore default bookmarks
        # UI changes
        "startup.homepage_welcome_url" = ""; # disable welcome page
        "browser.newtabpage.enabled" = false; # disable new tab page
        "full-screen-api.ignore-widgets" = true; # fullscreen within window
        "browser.toolbars.bookmarks.visibility" = "never"; # hide bookmarks toolbar
        "browser.aboutConfig.showWarning" = false; # disable warning about about:config
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = true; # disable picture in picture button

        # Privacy
        # "browser.discovery.enabled" = false; # disable discovery
        # "browser.search.suggest.enabled" = false; # disable search suggestions
        # "browser.contentblocking.category" = "custom"; # set tracking protection to custom
        "dom.private-attribution.submission.enabled" = false; # stop doing dumb stuff mozilla
        "browser.protections_panel.infoMessage.seen" = true; # disable tracking protection info

        # Disable telemetry
        "toolkit.telemetry.enabled" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "svg.context-properties.content.enabled" = true;
        "toolkit.telemetry.unified" = false;
        "browser.ping-centre.telemetry" = false;
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "browser.translations.automaticallyPopup" = false;
        "toolkit.telemetry.hybridContent.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.reportingpolicy.firstRun" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;

        # let me close and open tabs without confirmation
        "browser.tabs.loadInBackground" = true; # open new tab in background
        "browser.tabs.loadBookmarksInTabs" = true; # open bookmarks in new tab
        "browser.tabs.warnOnOpen" = false; # don't warn when opening multiple tabs
        "browser.tabs.warnOnQuit" = false; # don't warn when closing multiple tabs
        "browser.tabs.warnOnClose" = false; # don't warn when closing multiple tabs
        "browser.tabs.loadDivertedInBackground" = false; # open new tab in background
        "browser.tabs.warnOnCloseOtherTabs" = false; # don't warn when closing multiple tabs
        "browser.tabs.closeWindowWithLastTab" = false; # don't close window when last tab is closed

        # other
        "media.rdd-vpx.enabled" = true; # enable hardware acceleration
        "devtools.cache.disabled" = true; # disable caching in devtools
        "media.ffmpeg.vaapi.enabled" = true; # enable hardware acceleration

        # Fonts
        "browser.display.use_document_fonts" = 1;
        "browser.link.open_newwindow.restriction" = 0;

        "browser.fixup.domainsuffixwhitelist.home" = true;
        "browser.fixup.domainwhitelist.server.home" = true;
        # "keyword.enable" = false; # Disable search when typing unexistent TLD
      };
    };
  };

  stylix.targets.librewolf = {
    enable = true;
    profileNames = [ "default" ];
    colorTheme.enable = true;
  };

}

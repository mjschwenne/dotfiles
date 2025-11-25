{
  config,
  pkgs,
  ...
} @ inputs: let
  emacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./mjs/init.el;

    # Whether to include your config as a default init file.
    # If being bool, the value of config is used.
    # Its value can also be a derivation like this if you want to do some
    # substitution:
    #   defaultInitFile = pkgs.substituteAll {
    #     name = "default.el";
    #     src = ./emacs.el;
    #     inherit (config.xdg) configHome dataHome;
    #   };
    defaultInitFile = false;

    package = pkgs.emacs-unstable-pgtk;

    # By default emacsWithPackagesFromUsePackage will only pull in
    # packages with `:ensure`, `:ensure t` or `:ensure <package name>`.
    # Setting `alwaysEnsure` to `true` emulates `use-package-always-ensure`
    # and pulls in all use-package references not explicitly disabled via
    # `:ensure nil` or `:disabled`.
    # Note that this is NOT recommended unless you've actually set
    # `use-package-always-ensure` to `t` in your config.
    alwaysEnsure = true;

    # For Org mode babel files, by default only code blocks with
    # `:tangle yes` are considered. Setting `alwaysTangle` to `true`
    # will include all code blocks missing the `:tangle` argument,
    # defaulting it to `yes`.
    # Note that this is NOT recommended unless you have something like
    # `#+PROPERTY: header-args:emacs-lisp :tangle yes` in your config,
    # which defaults `:tangle` to `yes`.
    alwaysTangle = true;

    override = epkgs:
      epkgs
      // {
        org-timeblock = pkgs.callPackage ./emacs-pkgs/org-timeblock.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild org-ql persist;
        };
        org-modern-indent = pkgs.callPackage ./emacs-pkgs/org-modern-indent.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild;
        };
        typst-ts-mode = pkgs.callPackage ./emacs-pkgs/typst-ts-mode.nix {
          inherit (pkgs) fetchFromGitea;
          inherit (epkgs) trivialBuild;
        };
        typst-preview = pkgs.callPackage ./emacs-pkgs/typst-preview.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild websocket;
        };
      };

    # Optionally provide extra packages not in the configuration file.
    extraEmacsPackages = epkgs:
      [
        epkgs.org-modern-indent
        epkgs.autothemer
        epkgs.vterm
        epkgs.treesit-grammars.with-all-grammars
        epkgs.peg
        epkgs.typst-preview
      ]
      ++ (config.programs.emacs.extraPackages epkgs);
  };
in {
  programs.emacs = {
    enable = true;
    package = emacs;
  };

  services.emacs = {
    enable = true;
    startWithUserSession = "graphical";
  };

  home.file = {
    ".emacs.d" = {
      source = ./mjs;
      recursive = true;
    };
    ".emacs.d/logo.webp".source = ./mjs/emacs.webp;
    ".emacs.d/words.txt".source = ./lang-english.txt;
  };
}

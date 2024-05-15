{
  config,
  pkgs,
  builtins,
  ...
} @ inputs: {
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
  ];

  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      # Your Emacs config file. Org mode babel files are also
      # supported.
      # NB: Config files cannot contain unicode characters, since
      #     they're being parsed in nix, which lacks unicode
      #     support.
      # config = ./emacs.org;
      config = ./init.el;

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

      # Package is optional, defaults to pkgs.emacs
      package = pkgs.emacs-unstable-pgtk.override {};

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
          org-timeblock = pkgs.callPackage ./org-timeblock.nix {
            inherit (pkgs) fetchFromGitHub;
            inherit (epkgs) trivialBuild org-ql persist;
          };
          calfw-blocks = pkgs.callPackage ./calfw-blocks.nix {
            inherit (pkgs) fetchFromGitHub;
            inherit (epkgs) trivialBuild calfw calfw-org;
          };
          # org-auctex = pkgs.callPackage ./org-auctex.nix {
          #   inherit (epkgs) trivialBuild auctex;
          # };
        };

      # Optionally provide extra packages not in the configuration file.
      extraEmacsPackages = epkgs: [
        epkgs.org-timeblock
        epkgs.calfw-blocks
        # epkgs.org-auctex
      ];
    })
  ];

  home.file = {
    ".emacs.d/init.el".source = ./init.el;
    ".emacs.d/snippets" = {
      source = ./snippets;
      recursive = true;
    };
    ".emacs.d/autoloads" = {
      source = ./autoloads;
      recursive = true;
    };
    ".emacs.d/logo.webp".source = ./emacs.webp;
  };
}

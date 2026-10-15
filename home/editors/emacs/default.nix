#
# Emacs is built by home-manager's `programs.emacs` module and nothing else, so
# that exactly ONE wrapper exists.
#
# The tempting arrangement -- build a wrapper with
# `pkgs.emacsWithPackagesFromUsePackage` and assign it to `programs.emacs.package`
# -- silently breaks package dependencies. home-manager does:
#
#     emacsPackages = (pkgs.emacsPackagesFor cfg.package).overrideScope cfg.overrides;
#     finalPackage  = emacsPackages.emacsWithPackages extraPackages;
#
# so if `cfg.package` is already a wrapper, every package in `extraPackages` is
# byte-compiled using *that wrapper* as its build Emacs. A wrapper ships no
# `nix-support/setup-hook`, so `addEmacsVars` never runs, so `packageRequires`
# (= `propagatedBuildInputs`) never reaches EMACSLOADPATH. Dependencies then only
# resolve if they happen to be baked into the wrapper's own bundle, which makes
# them resolve by luck rather than by declaration. A flake update that drops one
# out of the bundle fails with a bare
# "Cannot open load file: ... base16-theme" and no hint as to the cause.
# The `-with-packages-with-packages` suffix on `finalPackage` is the tell.
#
# Passing the plain Emacs instead means packages build against an unwrapped
# Emacs that does carry the setup hook, so `packageRequires` is load-bearing.
#
# This also lets other modules contribute without any special handling, which is
# how stylix's Emacs target gets picked up: `extraPackages` is a
# `selectorFunction`, whose merge is `concatMap (select: select as) defs`, and
# `overrides` is an `overlayFunction`, which merges attrsets. Every module's
# definition is combined, so stylix's `base16-stylix-theme` package and its
# `extraConfig` arrive through the normal option system. Do NOT add
# `++ (config.programs.emacs.extraPackages epkgs)` here -- that self-reference is
# only needed to feed other modules' packages into a *second* wrapper, and
# reintroduces the problem above.
#
# The one thing lost by not calling `emacsWithPackagesFromUsePackage` is the
# init.el scan, so the scanner is invoked directly below.
#
{
  pkgs,
  lib,
  emacs-overlay,
  ...
}:
let
  # emacs-overlay exposes only `emacsWithPackagesFromUsePackage` on `pkgs`, not
  # the parser behind it, so import it from the flake input. It is available as a
  # module argument because the flake sets `extraSpecialArgs = inputs // { ... }`.
  parse = pkgs.callPackage "${emacs-overlay}/parse.nix" { };

  # Package names from the `use-package` declarations in init.el. `alwaysEnsure`
  # emulates `use-package-always-ensure`, matching what the config expects;
  # declarations marked `:ensure nil` (e.g. org-timeblock) are skipped.
  usePackageNames = parse.parsePackagesFromUsePackage {
    configText = builtins.readFile ./mjs/init.el;
    alwaysEnsure = true;
    alwaysTangle = true;
  };
in
{
  programs.emacs = {
    enable = true;

    # Must stay unwrapped -- see the comment at the top of this file.
    package = pkgs.emacs-unstable-pgtk;

    # Packages not in nixpkgs, or needing different inputs. Takes `super` rather
    # than a single argument so it composes as an overlay with other modules.
    overrides = _self: super: {
      org-timeblock = pkgs.callPackage ./emacs-pkgs/org-timeblock.nix {
        inherit (pkgs) fetchFromGitHub;
        inherit (super) trivialBuild org-ql persist;
      };
      org-modern-indent = pkgs.callPackage ./emacs-pkgs/org-modern-indent.nix {
        inherit (pkgs) fetchFromGitHub;
        inherit (super) trivialBuild;
      };
      typst-ts-mode = pkgs.callPackage ./emacs-pkgs/typst-ts-mode.nix {
        inherit (pkgs) fetchFromGitea;
        inherit (super) trivialBuild;
      };
      typst-preview = pkgs.callPackage ./emacs-pkgs/typst-preview.nix {
        inherit (pkgs) fetchFromGitHub;
        inherit (super) trivialBuild websocket;
      };
      markdown-modern = pkgs.callPackage ./emacs-pkgs/markdown-modern.nix {
        inherit (pkgs) fetchFromGitHub;
        inherit (super) trivialBuild;
      };
      consult-ripfd = pkgs.callPackage ./emacs-pkgs/consult-ripfd.nix {
        inherit (pkgs) fetchFromGitHub;
        inherit (super) trivialBuild consult;
      };
      emacs-reader = pkgs.callPackage ./emacs-pkgs/emacs-reader.nix {
        inherit (pkgs)
          fetchFromGitea
          gcc
          gnumake
          pkg-config
          mupdf-headless
          ;
        inherit (super) melpaBuild;
      };
    };

    # Everything from init.el, plus the packages that aren't declared there.
    # Other modules (stylix) append their own packages to this option.
    extraPackages =
      epkgs:
      let
        # `alwaysEnsure` means built-ins and `:ensure nil` entries show up as
        # names with no corresponding derivation; warn and drop them rather than
        # failing the whole build.
        fromUsePackage = lib.filter (p: p != null) (
          map (
            name:
            epkgs.${name}
              or (builtins.trace "Emacs package ${name}, declared wanted with use-package, not found." null)
          ) usePackageNames
        );
      in
      fromUsePackage
      ++ [
        epkgs.org-modern-indent
        epkgs.autothemer
        epkgs.vterm
        epkgs.treesit-grammars.with-all-grammars
        epkgs.peg
        epkgs.typst-preview
        epkgs.markdown-modern
        epkgs.consult-ripfd
        epkgs.emacs-reader
      ];
  };

  home.packages = with pkgs; [
    emacsPackages.pdf-tools
    imagemagick
  ];

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

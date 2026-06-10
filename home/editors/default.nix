{ pkgs, ... }:
{
  imports = [
    ./emacs
    ./nvim
    ./zed
    ./vscodium
    ./helix
  ];

  # System-wide checkers and linters shared between nvim and emacs
  home.packages = with pkgs; [
    # nix
    nixd
    nixfmt

    # prose
    proselint
    harper
    codebook
    rumdl

    # make
    checkmake

    # java
    google-java-format

    # python
    ty
    ruff
    python3Packages.python-lsp-server

    # typst
    tinymist
    typstyle

    # shell
    shfmt
    bash-language-server
    fish-lsp

    # markdown
    marksman

    # yaml
    yaml-language-server
  ];

  xdg.configFile = {
    "rumdl/rumdl.toml".text = # toml
      ''
        [global]
        extend-enable = ["MD060", "MD063"]

        [MD013]
        line-length = 80
        reflow = true
        reflow-mode = "normalize"
      '';
    "proselint/config.json".text = # json
      ''
        {
          "checks": {
            "annotations.misc": false,
            "lexical_illusions.misc": false,
            "typography.symbols": false
          }
        }
      '';
  };
}

{ pkgs, ... }:
{
  imports = [
    ./emacs
    ./nvim
    ./zed
    ./vscodium
  ];

  # System-wide checkers and linters shared between nvim and emacs
  home.packages = with pkgs; [
    alejandra
    proselint
    statix
    checkmake
    google-java-format
    deadnix
    nixfmt
    tinymist
    mypy
    black
    isort
    shfmt
    stylua

    # Testing a new editor
    steelix
  ];

  xdg.configFile."proselint/config.json".text =
    # json
    ''
      {
        "checks": {
          "annotations.misc": false,
          "lexical_illusions.misc": false,
          "typography.symbols": false
        }
      }
    '';
}

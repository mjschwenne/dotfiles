{pkgs, ...}: {
  imports = [./emacs ./nvim];

  # System-wide checkers and linters shared between nvim and emacs
  packages = with pkgs; [
    alejandra
    proselint
    statix
    checkmake
    google-java-format
    deadnix
    mypy
    black
    isort
    shfmt
    stylua
  ];

  xdg.configFile."proselint/config.json".text =
    /*
    json
    */
    ''
      {
        "checks": {
          "annotations.misc": false,
          "lexical_illusions.misc": false,
        }
      }
    '';
}

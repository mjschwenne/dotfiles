{ pkgs, ... }:
{
  programs.helix = {
    enable = true;
    package = pkgs.steelix;
    settings = {
      editor = {
        auto-format = true;
        line-number = "relative";
        cursorline = true;
        trim-trailing-whitespace = true;
        lsp.display-messages = true;
        cursor-shape = {
          insert = "bar";
        };
        indent-guides = {
          render = true;
        };
      };
    };
  };

  xdg.configFile."helix/languages.toml".text = ''
    [language-server.tinymist]
    command = "${pkgs.tinymist}/bin/tinymist"
    config.exportPdf = "onSave"
    config.preview.background.enabled = true
    config.preview.background.args = [
       "--data-plane-host=127.0.0.1:0", "--invert-colors=never", "--open"
    ]
    config.lint.enabled = true
    config.formatterProseWrap = true
    config.formatterPrintWidth = 80

    [language-server.codebook]
    command = "codebook-lsp"
    args = ["serve"]

    [language-server.harper-ls]
    command = "harper-ls"
    args = ["--stdio"]

    [[language]]
    name = "nix"
    auto-format = true

    [[language]]
    name = "typst"
    language-servers = ["tinymist", "codebook", "harper-ls"]
    auto-format = true
    soft-wrap = { enable = true }
  '';
}

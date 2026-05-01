{ pkgs, ... }:
{
  programs.helix = {
    enable = true;
    package = pkgs.steelix;
    settings = {
      editor = {
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
    # languages = {
    #   laguage-server.tinymist = {
    #     command = "${pkgs.tinymist}/bin/tinymist";
    #     config = {
    #       exportPdf = "onSave";
    #       preview.background.enabled = true;
    #       preview.background.args = [
    #         "--data-plane-host=127.0.0.1:0"
    #         "--invert-colors=never"
    #         "--open"
    #       ];
    #     };
    #   };
    # };
  };

  xdg.configFile."helix/languages.toml".text = ''
    [language-server.tinymist]
    command = "${pkgs.tinymist}/bin/tinymist"
    config.exportPdf = "onSave"
    config.preview.background.enabled = true
    config.preview.background.args = [
       "--data-plane-host=127.0.0.1:0", "--invert-colors=never", "--open"
    ]
  '';
}

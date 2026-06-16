{ pkgs, ... }:
let
  tree-sitter-cooklang = pkgs.fetchFromGitHub {
    owner = "cooklang";
    repo = "tree-sitter-cooklang";
    rev = "e156545d31b807ebd765f60f17eab968c94e2964";
    hash = "sha256-VeAHKf2qp2ApKMXWEqI8LUA9KYvxlZL/u6TlOlfmKLs=";
  };
in
{
  xdg.configFile."helix/runtime/queries/cooklang".source = "${tree-sitter-cooklang}/queries";

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

  xdg.configFile."helix/languages.toml".text = # toml
    ''
      [language-server.tinymist]
      command = "${pkgs.tinymist}/bin/tinymist"
      config.exportPdf = "never" # "onSave"
      config.preview.background.enabled = false # true
      config.preview.background.args = [
         "--data-plane-host=127.0.0.1:0", "--invert-colors=never", "--open"
      ]
      config.preview.browsing.args = [
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
      name = "markdown"
      auto-format = true
      soft-wrap = { enable = true }

      [[language]]
      name = "typst"
      language-servers = ["tinymist", "codebook", "harper-ls"]
      auto-format = true
      soft-wrap = { enable = true }

      [[language]]
      name = "cooklang"
      scope = "text.cooklang"
      injection-regex = "cooklang"
      file-types = [ "cook" ]
      language-servers = [ "cookcli" ]
      auto-format = true
      formatter = { command = "fmt", args = ["-w", "80", "-s"] }

      [language-server.cookcli]
      command = "cook"
      args = [ "lsp" ]
    '';
}

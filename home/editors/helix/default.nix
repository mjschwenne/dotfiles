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
  };
}

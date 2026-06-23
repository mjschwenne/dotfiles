{ ... }:
{
  programs.zathura = {
    enable = true;
    options = {
      selection-clipboard = "clipboard";
      page-cache-size = 1024;
    };
  };
}

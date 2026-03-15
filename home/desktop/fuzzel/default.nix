{ lib, ... }:
{
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        terminal = "ghostty -e '{cmd}'";
        icons-enabled = true;
        line-height = 30; # To get slightly larger icons...
        width = 40;
        lines = 10;
        horizontal-pad = 20;
        vertical-pad = 8;
        inner-pad = 6;
        match-mode = "fuzzy";
        show-actions = true;
        layer = "overlay";
        font = lib.mkForce "JetBrainsMono Nerd Font:size=14";
        prompt = "\"  \"";
      };

      border = {
        width = 3;
        radius = 5;
      };
    };
  };
}

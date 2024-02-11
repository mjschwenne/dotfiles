{...}: {
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    settings = {
      add_newline = false;
      format = ''
        $time$directory$nix_shell$git_branch
        $character
      '';
      character = {
        success_symbol = "[• ](bold fg:blue) ";
        error_symbol = "[• 󰅙](bold fg:red) ";
      };
      git_branch = {
        style = "bg: green";
        symbol = "󰘬";
        truncation_length = 4;
        truncation_symbol = "";
        format = " • [](bold fg:green)[$symbol $branch(:$remote_branch)](fg:black bg:green)[ ](bold fg:green)";
      };
      hostname = {
        ssh_only = false;
        format = "[•$hostname](bg:cyan bold fg:black)[](bold fg:cyan )";
        trim_at = ".cs.wisc.edu";
        disabled = false;
      };
      line_break = {
        disabled = false;
      };
      username = {
        style_user = "bold bg:cyan fg:black";
        style_root = "red bold";
        format = "[](bold fg:cyan)[$user]($style)[](bold fg:cyan)";
        disabled = false;
        show_always = true;
      };
      directory = {
        home_symbol = "  ";
        read_only = "  ";
        style = "bg:blue fg:black";
        truncation_length = 6;
        truncation_symbol = "••/";
        format = "[](bold fg:blue)[$path ]($style)[](bold fg:blue)";
      };
      directory.substitutions = {
        "Desktop" = "  ";
        "Documents" = "  ";
        "Downloads" = "  ";
        "Music" = " 󰎈 ";
        "Pictures" = "  ";
        "Videos" = "  ";
      };
      time = {
        disabled = false;
        time_format = "%R";
        format = "[](bold fg:yellow)[ $time](bold bg:yellow fg:black)[](bold fg:yellow) •• ";
      };
      nix_shell = {
        symbol = "󱄅 ";
        style = "bg:blue fg:black";
        format = " • [](bold fg:blue)[ $symbol]($style)[](bold fg:blue)";
      };
    };
  };
}

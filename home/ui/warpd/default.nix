{pkgs, ...}: {
  home.packages = with pkgs; [
    (warpd.overrideAttrs (old: {
      version = "1.3.7";
      src = pkgs.fetchFromGitHub {
        owner = "rvaiya";
        repo = "warpd";
        rev = "01650eabf70846deed057a77ada3c0bbb6d97d6e";
        hash = "sha256-TUNV8yvbASI1F17f6LGMQbQySFNhqDhrN74fRlpABhQ=";
        leaveDotGit = true;
      };
    }))
  ];

  xdg.configFile."warpd/config".text = let
    love = "#eb6f92";
    gold = "#f6c177";
    rose = "#ebbcba";
    pine = "#31748f";
    foam = "#9ccfd8";
    iris = "#c4a7e7";
    text = "#e0def4";
    base = "#191724";
  in ''
    hint_activation_key: unbind
    hint2_activation_key: unbind
    grid_activation_key: unbind
    history_activation_key: unbind
    screen_activation_key: unbind
    activation_key: unbind
    hint_oneshot_key: unbind
    hint2_oneshot_key: unbind

    grid_color: ${gold}
    hint_bgcolor: ${iris}
    hint_fgcolor: ${base}
    hint_font: JetBrains Mono Nerd Font 10
    cursor_color: ${love}
  '';
}

{
  osConfig,
  pkgs,
  ...
}:
{
  programs.nushell = {
    enable = true;
    configFile.text = /* nu */ ''
      if $nu.is-interactive {
          fastfetch -c examples/8.jsonc
      }
    '';
    environmentVariables = {
      SHELL = "${pkgs.nushell}/bin/nu";
    };
    extraConfig = ''
      source ~/.config/nushell/zoxide.nu
    '';
    extraEnv = ''
      zoxide init nushell | save -f ~/.config/nushell/zoxide.nu
    '';
    shellAliases = {
      vi = "nvim";
      ll = "ls -l";
      icat = "wezterm imgcat";
      nix-dev = ''nix develop -c "nu"'';
      # ssh = ''kitty +kitten ssh -i ${osConfig.sops.secrets."${osConfig.networking.hostName}/ssh/key".path}'';
      scp = ''scp -i ${osConfig.sops.secrets."${osConfig.networking.hostName}/ssh/key".path}'';
    };
    settings = {
      buffer_editor = "nvim";
      completions = {
        algorithm = "fuzzy";
        case_sensitive = false;
        partial = true;
        quick = true;
      };
      menus = [
        {
          name = "completion_menu";
          only_buffer_difference = false;
          marker = "   ";
          type = {
            layout = "columnar";
            columns = 4;
            col_width = 20;
            col_padding = 2;
          };
          style = {
            text = "green";
            selected_text = "green_reverse";
            description_text = "yellow";
          };
        }
      ];
      show_banner = false;
      plugins = {
        highlight.theme = "Nord";
      };
    };
    plugins = with pkgs.nushellPlugins; [
      polars
      query
      formats
      # highlight
      # desktop_notifications
    ];
  };
}

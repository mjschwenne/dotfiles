{
  osConfig,
  pkgs,
  ...
}: {
  programs.nushell = {
    enable = true;
    configFile.text =
      /*
      nu
      */
      ''
        if $nu.is-interactive {
            fastfetch -c examples/8.jsonc
        }
      '';
    extraConfig = ''
      source ~/.config/nushell/zoxide.nu
    '';
    extraEnv = ''
      zoxide init nushell | save -f ~/.config/nushell/zoxide.nu
    '';
    shellAliases = {
      vi = "nvim";
      icat = "wezterm imgcat";
      nix-shell = "nix-shell --run nu";
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
      show_banner = false;
    };
    plugins = with pkgs.nushellPlugins; [
      polars
      query
      formats
      highlight
      desktop_notifications
    ];
  };
}

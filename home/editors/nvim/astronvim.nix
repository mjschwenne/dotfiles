{pkgs, ...}: {
  home = {
    file = {
      ".config/astronvim" = {
        source = ./astronvim;
        recursive = true;
        # onChange = "nvim --headless -c 'if exists(\":LuaCacheClear\") | :LuaCacheClear' +quitall";
      };
    };

    packages = with pkgs; [
      # astronvim deps
      gdu
      lazygit
      tree-sitter
    ];
  };

  # programs.neovim = {
  #   enable = true;
  #   viAlias = true;
  #   vimAlias = true;
  #
  #   plugins = with pkgs.vimPlugins; [
  #     telescope-fzf-native-nvim
  #   ];
  # };
}

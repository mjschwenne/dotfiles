{ pkgs, ...} :
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
    	# Appearance
		catppuccin-nvim
		feline-nvim
		alpha-nvim
    ];

    extraConfig = ''
      lua << EOF 
      ${builtins.readFile lua/options.lua}
      ${builtins.readFile lua/appearance.lua}
    '';
  };
}

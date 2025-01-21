{pkgs, ...}: {
  home = {
    file = {
      ".config/nvim" = {
        source = ./nvim;
        recursive = true;
        # onChange = "nvim --headless -c 'if exists(\":LuaCacheClear\") | :LuaCacheClear' +quitall";
      };
    };
    packages = with pkgs; [
      # astronvim deps
      gdu
      lazygit
      tree-sitter

      # Language Servers
      clang-tools
      haskell-language-server
      jdt-language-server
      ltex-ls
      lua-language-server
      nixd
      nil
      pyright
      rust-analyzer
      typescript-language-server
      bash-language-server
      texlab
      coqPackages.coq-lsp

      # language specific stuff
      lldb
      haskellPackages.fast-tags
      haskellPackages.haskell-debug-adapter
    ];
  };
}

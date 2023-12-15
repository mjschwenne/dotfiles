{pkgs, ...}: let
  whichkey = pkgs.vimUtils.buildVimPlugin {
    pname = "which-key.nvim";
    version = "2023-12-13";
    src = pkgs.fetchFromGitHub {
      owner = "folke";
      repo = "which-key.nvim";
      rev = "4433e5ec9a507e5097571ed55c02ea9658fb268a";
      sha256 = "uvghPj/teWrRMm09Gh8iQ/LV2nYJw0lmoiZK6L4+1cY=";
    };
  };
in {
  home.packages = with pkgs; [
    # Language Servers
    clang-tools
    haskell-language-server
    jdt-language-server
    ltex-ls
    lua-language-server
    nixd
    nodePackages.pyright
    rust-analyzer

    # none-ls sources
    alejandra
    proselint
    statix
    checkmake
    checkstyle
    deadnix
    luajitPackages.luacheck
    mypy
    black
    stylish-haskell
    haskellPackages.cabal-fmt
    isort
    rustfmt
    shfmt

    # language specific stuff
    lldb
    haskellPackages.fast-tags
    python311Packages.debugpy
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      # Appearance
      catppuccin-nvim
      alpha-nvim
      lualine-nvim
      lualine-lsp-progress

      # Utility
      whichkey
      nvim-autopairs
      nvim-colorizer-lua
      comment-nvim

      # File tree
      nvim-tree-lua

      # Completion
      coq_nvim
      coq-thirdparty
      coq-artifacts

      # LSP
      nvim-lspconfig
      lsp_lines-nvim
      trouble-nvim
      none-ls-nvim

      # Other Language Plugins
      vim-ledger
      vimtex
      rust-tools-nvim
      vim-nix
      vim-nixhash

      # DAP
      nvim-dap
      nvim-dap-virtual-text
      nvim-dap-python
      telescope-dap-nvim

      # Telescope
      plenary-nvim
      telescope-nvim
      telescope-fzf-native-nvim

      # Treesitter
      nvim-treesitter
      nvim-treesitter-parsers.bash
      nvim-treesitter-parsers.bibtex
      nvim-treesitter-parsers.c
      nvim-treesitter-parsers.commonlisp
      nvim-treesitter-parsers.cpp
      nvim-treesitter-parsers.css
      nvim-treesitter-parsers.csv
      nvim-treesitter-parsers.fish
      nvim-treesitter-parsers.haskell
      nvim-treesitter-parsers.java
      nvim-treesitter-parsers.julia
      nvim-treesitter-parsers.json
      nvim-treesitter-parsers.latex
      nvim-treesitter-parsers.ledger
      nvim-treesitter-parsers.lua
      nvim-treesitter-parsers.make
      nvim-treesitter-parsers.markdown
      nvim-treesitter-parsers.markdown_inline
      nvim-treesitter-parsers.nix
      nvim-treesitter-parsers.org
      nvim-treesitter-parsers.python
      nvim-treesitter-parsers.r
      nvim-treesitter-parsers.regex
      nvim-treesitter-parsers.rst
      nvim-treesitter-parsers.rust
      nvim-treesitter-parsers.vim
      nvim-treesitter-parsers.vimdoc
      nvim-treesitter-parsers.yuck
      nvim-treesitter-endwise
      nvim-treesitter-context
      nvim-treesitter-textobjects
      nvim-treesitter-textsubjects
      nvim-treesitter-refactor
    ];

    extraConfig = ''
         lua << EOF
         ${builtins.readFile lua/options.lua}
         ${builtins.readFile lua/appearance.lua}
      ${builtins.readFile lua/keymaps.lua}
      ${builtins.readFile lua/autopairs.lua}
      ${builtins.readFile lua/utilities.lua}
      ${builtins.readFile lua/tree.lua}
         ${builtins.readFile lua/treesitter.lua}
         ${builtins.readFile lua/telescope.lua}
         ${builtins.readFile lua/completions.lua}
         ${builtins.readFile lua/lsp.lua}
    '';
  };
}

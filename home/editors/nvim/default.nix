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
  home = {
    file = {
      ".config/nvim/init.lua" = {
        source = ./init.lua;
      };

      ".config/nvim/lua" = {
        source = ./lua;
        recursive = true;
      };

      ".config/nvim/after/ftplugin/haskell.lua" = {
        source = ./ftplugin/haskell.lua;
      };

      ".config/nvim/after/ftplugin/java.lua".text = ''
        local jdtls_p, jdtls = pcall(require, "jdtls")
        if not jdtls_p then
            print "Failed to load jdtls..."
            return
        end

        jdtls.start_or_attach({
            root_dir = vim.fn.getcwd(),
            cmd = {

                "java", -- or '/path/to/java17_or_newer/bin/java'
                -- depends on if `java` is in your $PATH env variable and if it points to the right version.

                "-Declipse.application=org.eclipse.jdt.ls.core.id1",
                "-Dosgi.bundles.defaultStartLevel=4",
                "-Declipse.product=org.eclipse.jdt.ls.core.product",
                "-Dlog.protocol=true",
                "-Dlog.level=ALL",
                "-Xmx1g",
                "--add-modules=ALL-SYSTEM",
                "--add-opens", "java.base/java.util=ALL-UNNAMED",
                "--add-opens", "java.base/java.lang=ALL-UNNAMED",

                -- Mark the config area as read-only
                "-Dosgi.sharedConfiguration.area.readOnly=true",
                "-Dosgi.checkConfiguration=true",
                "-Dosgi.configuration.cascaded=true",
                "-Dosgi.sharedConfiguration.area=${pkgs.jdt-language-server}/share/config",

                -- TODO Find a better way to specify this file name
                "-jar", "${pkgs.jdt-language-server}/share/java/plugins/org.eclipse.equinox.launcher_1.6.500.v20230717-2134.jar",
                -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                                       ^^^^^^^^^^^^^^
                -- Must point to the                                                     Change this to
                -- eclipse.jdt.ls installation                                           the actual version


                -- "-configuration", "~/.config/nvim/jdtls-config/",
                -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^        ^^^^^^
                -- Must point to the                      Change to one of `linux`, `win` or `mac`
                -- eclipse.jdt.ls installation            Depending on your system.

                -- See `data directory configuration` section in the README
                "-data", "~/.local/share/nvim/jdtls-workspace/"
            },
        })

        require("which-key").register({
          ['<leader'] = {
            ['<localleader>'] = {
              m = {
                name = "Java",
                e = {
                  name = "Extract",
                  c = { jdtls.extract_constant, "Extract Constant" },
                  v = { jdtls.extract_variable, "Extract Variable" },
                  V = { jdtls.extract_variable_all, "Extract Variable & Occurances" },
                  m = { jdtls.extract_method, "Extract Method" },
                },
                o = { jdtls.organize_imports, "Organize Imports" },
                c = { jdtls.compile, "Compile" },
                s = { jdtls.super_implementation, "Goto Super" },
              }
            }
          }
        })
      '';
    };

    packages = with pkgs; [
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
      google-java-format
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
      haskellPackages.haskell-debug-adapter
    ];
  };

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
      nvim-colorizer-lua

      # Utility
      whichkey
      nvim-autopairs
      comment-nvim

      # File tree
      nvim-tree-lua
      nvim-web-devicons

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
      nvim-jdtls
      vimtex
      rust-tools-nvim
      vim-nix
      vim-nixhash
      hmts-nvim
      yuck-vim
      haskell-tools-nvim

      # DAP
      nvim-dap
      nvim-dap-ui
      nvim-dap-virtual-text
      nvim-dap-python
      telescope-dap-nvim

      # Telescope
      plenary-nvim
      telescope-nvim
      telescope-fzf-native-nvim
      telescope-media-files-nvim
      telescope-ui-select-nvim

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
      nvim-ts-context-commentstring
    ];
  };
}

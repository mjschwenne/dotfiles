{
  pkgs,
  nvf,
  ...
}: {
  imports = [nvf.homeManagerModules.default];

  programs.nvf = {
    enable = true;
    settings.vim = {
      viAlias = true;
      vimAlias = true;
      extraPlugins = {
        htms = {
          package = pkgs.vimUtils.buildVimPlugin {
            name = "htms.nvim";
            src = pkgs.fetchFromGitHub {
              owner = "calops";
              repo = "htms.nvim";
              rev = "a32cd413f7d0a69d7f3d279c631f20cb117c8d30";
              hash = "sha256-j/RFJgCbaH+V2K20RrQbsz0bzpN8Z6YAKzZMABYg/OU=";
            };
          };
        };
      };
      autopairs.nvim-autopairs.enable = true;
      autocomplete.nvim-cmp.enable = true;
      binds = {
        whichKey.enable = true;
        cheatsheet.enable = true;
      };
      comments.comment-nvim.enable = true;
      dashboard.alpha.enable = true;
      debugger = {
        nvim-dap = {
          enable = true;
          ui.enable = true;
        };
      };
      # FIXME: get this to actually work
      # diagnostics = {
      #   enable = true;
      #   config.signs = {
      #     text = {
      #       "vim.diagnostic.severity.ERROR" = "󰅚 ";
      #       "vim.diagnostic.severity.WARN" = "󰀪 ";
      #     };
      #   };
      # };
      extraLuaFiles = [
        ./highlight-yank.lua
        ./diagonstic-signs.lua
      ];
      filetree.neo-tree.enable = true;
      git = {
        enable = true;
        gitsigns.enable = true;
        gitsigns.codeActions.enable = false; # Suppress debug messages
      };
      lsp = {
        enable = true;
        formatOnSave = true;
        lspkind.enable = true;
        lspsaga.enable = true;
        lspSignature.enable = true;
        otter-nvim.enable = true;
        nvim-docs-view.enable = true;
        trouble.enable = true;
      };
      languages = {
        enableFormat = true;
        enableTreesitter = true;
        enableExtraDiagnostics = true;

        go.enable = true;
        haskell.enable = true;
        python.enable = true;
        markdown.enable = true;
        nix.enable = true;
        ocaml.enable = true;
        rust = {
          enable = true;
          crates.enable = true;
        };
        lua.enable = true;
        yaml.enable = true;
      };
      maps = {
        normal = {
          "<C-h>" = {
            action = "<C-w>h";
            desc = "Move Focus Left";
          };
          "<C-j>" = {
            action = "<C-w>j";
            desc = "Move Focus Up";
          };
          "<C-k>" = {
            action = "<C-w>k";
            desc = "Move Focus Up";
          };
          "<C-l>" = {
            action = "<C-w>l";
            desc = "Move Focus Right";
          };
        };
      };
      notes.todo-comments.enable = true;
      notify.nvim-notify.enable = true;
      options = {
        autochdir = true;
        autoindent = true;
        backup = false;
        clipboard = "unnamedplus";
        fileencoding = "utf-8";
        history = 50;
        hlsearch = true;
        ignorecase = true;
        numberwidth = 4;
        pumheight = 10;
        shiftwidth = 4;
        scrolloff = 8;
        showmode = false;
        sidescrolloff = 8;
        smartcase = true;
        smartindent = true;
        splitbelow = true;
        splitright = true;
        tabstop = 4;
        timeoutlen = 500;
        wrap = false;
      };
      spellcheck.enable = true;
      snippets.luasnip.enable = true;
      statusline = {
        lualine.enable = true;
      };
      tabline.nvimBufferline.enable = true;
      telescope.enable = true;
      terminal = {
        toggleterm = {
          enable = true;
          lazygit.enable = true;
        };
      };
      theme = {
        enable = true;
        name = "nord";
      };
      treesitter = {
        enable = true;
        context.enable = true;
        grammars = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [just kdl];
      };
      ui = {
        borders.enable = true;
        noice.enable = true;
        colorizer.enable = true;
        illuminate.enable = true;
        breadcrumbs = {
          enable = true;
          navbuddy.enable = true;
        };
        smartcolumn = {
          enable = true;
          setupOpts.custom_colorcolumn = {
            nix = "110";
            go = ["90" "130"];
          };
        };
        fastaction.enable = true;
      };
      utility = {
        diffview-nvim.enable = true;
        surround.enable = true;

        motion = {
          hop.enable = true;
          leap.enable = true;
          precognition.enable = false;
        };
      };
      visuals = {
        nvim-scrollbar.enable = false;
        nvim-web-devicons.enable = true;
        nvim-cursorline.enable = true;
        cinnamon-nvim.enable = true;
        fidget-nvim.enable = true;

        # highlight-undo.enable = true;
        # indent-blankline.enable = true;
      };
    };
  };
}

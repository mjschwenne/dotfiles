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
      extraPlugins = let
        diagflow-nvim = pkgs.vimUtils.buildVimPlugin {
          pname = "diagflow.nvim";
          version = "2025-04-25";
          src = pkgs.fetchFromGitHub {
            owner = "dgagn";
            repo = "diagflow.nvim";
            rev = "b13321b517ff64bf42eeac2214085d3c76d83a0d";
            sha256 = "sha256-gJlM0diDmyvmW5l/QIpUe2bDTZg8XekLBcFOoxeUW4E=";
          };
          dependencies = [pkgs.vimPlugins.harpoon];
        };
      in {
        diagflow = {
          package = diagflow-nvim;
          setup = "require('diagflow').setup()";
        };
      };
      filetree.neo-tree.enable = true;
      git = {
        enable = true;
        gitsigns.enable = true;
        gitsigns.codeActions.enable = false; # Suppress debug messages
      };
      lsp = {
        enable = true;
        formatOnSave = true;
        lightbulb.enable = true;
        lsplines.enable = false;
        lspkind.enable = true;
        lspsaga.enable = true;
        lspSignature.enable = true;
        otter-nvim.enable = true;
        nvim-docs-view.enable = true;
        trouble.enable = true;
      };
      languages = {
        enableLSP = true;
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
        grammars = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [just];
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
          precognition.enable = true;
        };
      };
      visuals = {
        nvim-scrollbar.enable = true;
        nvim-web-devicons.enable = true;
        nvim-cursorline.enable = true;
        cinnamon-nvim.enable = true;
        fidget-nvim.enable = true;

        # highlight-undo.enable = true;
        indent-blankline.enable = true;
      };
    };
  };
}

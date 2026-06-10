{
  nixvim,
  ...
}:
{
  imports = [ nixvim.homeModules.nixvim ];

  programs.nixvim =
    let
      mkRaw = nixvim.lib.nixvim.mkRaw;
    in
    {
      enable = true;
      clipboard.register = "unnamedplus";
      diagnostic.settings = {
        signs.text = {
          "__rawKey__vim.diagnostic.severity.ERROR" = "";
          "__rawKey__vim.diagnostic.severity.WARN" = "";
          "__rawKey__vim.diagnostic.severity.INFO" = "";
          "__rawKey__vim.diagnostic.severity.HINT" = "";
        };
      };
      opts = {
        autochdir = true;
        autoindent = true;
        backup = false;
        fileencoding = "utf-8";
        history = 50;
        hlsearch = true;
        ignorecase = true;
        numberwidth = 4;
        pumheight = 10;
        shiftwidth = 4;
        scrolloff = 8;
        sidescrolloff = 8;
        showmode = false;
        smartindent = true;
        splitbelow = true;
        splitright = true;
        tabstop = 4;
        timeoutlen = 500;
        wrap = false;
      };
      lsp.servers = {
        nixd.enable = true;
        jdtls.enable = true;
        tinymist.enable = true;
      };
      plugins = {
        lspconfig.enable = true;
        tiny-inline-diagnostic = {
          enable = true;
          settings = {
            multilines = {
              enabled = true;
            };
            add_messages = {
              display_count = true;
            };
          };
        };
        lsp-format.enable = true;
        hardtime.enable = true;
        guess-indent.enable = true;
        lz-n.enable = true;
        numbertoggle.enable = true;
        nix.enable = true;
        nix-develop.enable = true;
        friendly-snippets.enable = true;
        mini = {
          enable = true;
          modules = {
            move = { };
            splitjoin = { };
            ai = { };
            comment = { };
            extra = { };
            files = { };
            pick = { };
            trailspace = { };
            statusline = { };
            bracketed = { };
            clue = {
              triggers = [
                {
                  mode = [
                    "n"
                    "x"
                  ];
                  keys = "<leader>";
                }
                {
                  mode = "n";
                  keys = "[";
                }
                {
                  mode = "i";
                  keys = "<C-x>";
                }
                {
                  mode = [
                    "n"
                    "x"
                  ];
                  keys = "g";
                }
                {
                  mode = [
                    "n"
                    "x"
                  ];
                  keys = "'";
                }
                {
                  mode = [
                    "n"
                    "x"
                  ];
                  keys = "`";
                }
                {
                  mode = [
                    "n"
                    "x"
                  ];
                  keys = ''"'';
                }
                {
                  mode = [
                    "i"
                    "c"
                  ];
                  keys = "<C-r>";
                }
                {
                  mode = "n";
                  keys = "<C-w>";
                }
                {
                  mode = [
                    "n"
                    "x"
                  ];
                  keys = "z";
                }
              ];
              clues = [
                (mkRaw ''require("mini.clue").gen_clues.square_brackets()'')
                (mkRaw ''require("mini.clue").gen_clues.builtin_completion()'')
                (mkRaw ''require("mini.clue").gen_clues.g()'')
                (mkRaw ''require("mini.clue").gen_clues.marks()'')
                (mkRaw ''require("mini.clue").gen_clues.registers()'')
                (mkRaw ''require("mini.clue").gen_clues.windows()'')
                (mkRaw ''require("mini.clue").gen_clues.z()'')
              ];
            };
            indentscope = { };
            jump = { };
            jump2d = { };
            tabline = { };
            doc = { };
            pairs = { };
            icons = { };
            git = { };
            diff = {
              view = {
                style = "sign";
                signs = {
                  add = "┃";
                  change = "┃";
                  delete = "┃";
                };
              };
            };
            snippets = { };
            completion = { };
            basics = {
              mappings = {
                windows = true;
                move_with_alt = true;
              };
            };
            hipatterns = {
              highlighers = {
                fixme = {
                  pattern = "%f[%w]()FIXME()%f[%W]";
                  group = "MiniHipatternsFixme";
                };
                hack = {
                  pattern = "%f[%w]()HACK()%f[%W]";
                  group = "MiniHipatternsHack";
                };
                todo = {
                  pattern = "%f[%w]()TODO()%f[%W]";
                  group = "MiniHipatternsTodo";
                };
                note = {
                  pattern = "%f[%w]()NOTE()%f[%W]";
                  group = "MiniHipatternsNote";
                };
                hex_color = {
                  __raw = /* lua */ ''require("mini.hipatterns").gen_highlighter.hex_color()'';
                };
              };
            };
            starter = {
              content_hooks = {
                "__unkeyed-1.adding_bullet" = {
                  __raw = "require('mini.starter').gen_hook.adding_bullet()";
                };
                "__unkeyed-2.indexing" = {
                  __raw = "require('mini.starter').gen_hook.indexing('all', { 'Builtin actions' })";
                };
                "__unkeyed-3.padding" = {
                  __raw = "require('mini.starter').gen_hook.aligning('center', 'center')";
                };
              };
              evaluate_single = true;
              header = ''
                ███╗   ██╗██╗██╗  ██╗██╗   ██╗██╗███╗   ███╗
                ████╗  ██║██║╚██╗██╔╝██║   ██║██║████╗ ████║
                ██╔██╗ ██║██║ ╚███╔╝ ██║   ██║██║██╔████╔██║
                ██║╚██╗██║██║ ██╔██╗ ╚██╗ ██╔╝██║██║╚██╔╝██║
                ██║ ╚████║██║██╔╝ ██╗ ╚████╔╝ ██║██║ ╚═╝ ██║
              '';
              items = {
                "__unkeyed-1.buildtin_actions" = {
                  __raw = "require('mini.starter').sections.builtin_actions()";
                };
                "__unkeyed-2.recent_files_current_directory" = {
                  __raw = "require('mini.starter').sections.recent_files(10, false)";
                };
                "__unkeyed-3.recent_files" = {
                  __raw = "require('mini.starter').sections.recent_files(10, true)";
                };
                "__unkeyed-4.sessions" = {
                  __raw = "require('mini.starter').sections.sessions(5, true)";
                };
              };
            };
          };
        };
        treesitter = {
          enable = true;
          highlight.enable = true;
          indent.enable = true;
          folding.enable = false;
        };
        java.enable = true;
        spring-boot.enable = true; # dep of java-nvim plugin
      };
    };

  #   programs.nvf = {
  #     enable = true;
  #     settings.vim = {
  #       viAlias = true;
  #       vimAlias = false;
  #       theme.transparent = true;
  #       extraPlugins = {
  #         transparent = {
  #           package = pkgs.vimUtils.buildVimPlugin {
  #             name = "transparent.nvim";
  #             src = pkgs.fetchFromGitHub {
  #               owner = "xiyaowong";
  #               repo = "transparent.nvim";
  #               rev = "8ac59883de84e9cd1850ea25cf087031c5ba7d54";
  #               hash = "sha256-GlN7/+TmXld2UVPN2rDP7nKqbnswiezmGXn+uGK5I5c=";
  #             };
  #           };
  #           setup = /* lua */ ''
  #             require("transparent").setup({
  #               extra_groups = {
  #                 "NormalFloat",
  #                 "GitGutterAdd",
  #                 "GitGutterChange",
  #                 "GitGutterDelete",
  #                 "GitGutterChangeDelete"
  #                }
  #             })'';
  #         };
  #       };
  #       autocmds = [
  #         {
  #           enable = true;
  #           desc = "Highlight text as it is yanked";
  #           event = [ "TextYankPost" ];
  #           pattern = [ "*" ];
  #           callback =
  #             lib.generators.mkLuaInline # lua
  #               ''
  #                 function ()
  #                     vim.highlight.on_yank({higroup="Visual", timeout=200})
  #                 end
  #               '';
  #         }
  #         {
  #           enable = true;
  #           desc = "Show LSP diagnostics in float";
  #           event = [ "CursorHold" ];
  #           pattern = [ "*" ];
  #           callback =
  #             lib.generators.mkLuaInline # lua
  #
  #               ''
  #                 function ()
  #                     for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
  #                         if vim.api.nvim_win_get_config(winid).zindex then
  #                           return
  #                         end
  #                     end
  #                     vim.diagnostic.open_float({
  #                         scope = "cursor",
  #                         focusable = false,
  #                         close_events = {
  #                             "CursorMoved",
  #                             "CursorMovedI",
  #                             "BufHidden",
  #                             "InsertCharPre",
  #                             "WinLeave",
  #                         },
  #                     })
  #                 end
  #               '';
  #         }
  #       ];
  #       autopairs.nvim-autopairs.enable = true;
  #       autocomplete.nvim-cmp.enable = true;
  #       binds = {
  #         whichKey = {
  #           enable = true;
  #           register = {
  #             "<leader>a" = "Artifical Intelligence";
  #             "<leader>c" = "Conflicts";
  #             "<leader>d" = "Debug";
  #             "<leader>l" = "Language";
  #             "<leader>t" = "Toggle";
  #           };
  #         };
  #         cheatsheet.enable = true;
  #       };
  #       comments.comment-nvim.enable = true;
  #       dashboard.alpha.enable = true;
  #       debugger = {
  #         nvim-dap = {
  #           enable = true;
  #           ui.enable = true;
  #         };
  #       };
  #       diagnostics = {
  #         enable = true;
  #         config = {
  #           signs.text =
  #             lib.generators.mkLuaInline # lua
  #
  #               ''
  #                 {
  #                   [vim.diagnostic.severity.ERROR] = "󰅚 ",
  #                   [vim.diagnostic.severity.WARN] = "󰀪 ",
  #                 }
  #               '';
  #           float.border = "rounded";
  #         };
  #       };
  #       filetree.neo-tree.enable = true;
  #       git = {
  #         enable = true;
  #         gitsigns.enable = true;
  #         gitsigns.codeActions.enable = false; # Suppress debug messages
  #       };
  #       lsp = {
  #         enable = true;
  #         formatOnSave = true;
  #         lspkind.enable = true;
  #         lspsaga = {
  #           enable = true;
  #           setupOpts.lightbulb.enable = false;
  #         };
  #         lspSignature.enable = true;
  #         otter-nvim.enable = true;
  #         nvim-docs-view.enable = true;
  #         trouble.enable = true;
  #       };
  #       formatter.conform-nvim = {
  #         enable = true;
  #       };
  #       treesitter.grammars = pkgs.vimPlugins.nvim-treesitter.allGrammars;
  #       languages = {
  #         enableTreesitter = true;
  #         enableExtraDiagnostics = true;
  #
  #         go.enable = true;
  #         haskell.enable = true;
  #         java = {
  #           enable = true;
  #           lsp.enable = true;
  #         };
  #         markdown.enable = true;
  #         nix = {
  #           enable = true;
  #           lsp.servers = [ "nixd" ];
  #         };
  #         ocaml = {
  #           enable = true;
  #         };
  #         python.enable = true;
  #         rust = {
  #           enable = true;
  #           extensions.crates-nvim = {
  #             enable = true;
  #           };
  #         };
  #         lua.enable = true;
  #         yaml.enable = true;
  #       };
  #       maps = {
  #         normal = {
  #           "<C-h>" = {
  #             action = "<C-w>h";
  #             desc = "Move Focus Left";
  #           };
  #           "<C-j>" = {
  #             action = "<C-w>j";
  #             desc = "Move Focus Up";
  #           };
  #           "<C-k>" = {
  #             action = "<C-w>k";
  #             desc = "Move Focus Up";
  #           };
  #           "<C-l>" = {
  #             action = "<C-w>l";
  #             desc = "Move Focus Right";
  #           };
  #         };
  #       };
  #       notes.todo-comments.enable = true;
  #       notify.nvim-notify = {
  #         enable = true;
  #         setupOpts.background_colour = "#000000";
  #       };
  #       options = {
  #         autochdir = true;
  #         autoindent = true;
  #         backup = false;
  #         clipboard = "unnamedplus";
  #         fileencoding = "utf-8";
  #         history = 50;
  #         hlsearch = true;
  #         ignorecase = true;
  #         numberwidth = 4;
  #         pumheight = 10;
  #         shiftwidth = 4;
  #         scrolloff = 8;
  #         showmode = false;
  #         sidescrolloff = 8;
  #         smartcase = true;
  #         smartindent = true;
  #         splitbelow = true;
  #         splitright = true;
  #         tabstop = 4;
  #         timeoutlen = 500;
  #         wrap = false;
  #       };
  #       spellcheck.enable = true;
  #       snippets.luasnip.enable = true;
  #       statusline = {
  #         lualine.enable = true;
  #       };
  #       tabline.nvimBufferline.enable = true;
  #       telescope.enable = true;
  #       terminal = {
  #         toggleterm = {
  #           enable = true;
  #           lazygit.enable = true;
  #         };
  #       };
  #       ui = {
  #         borders.enable = true;
  #         noice.enable = true;
  #         colorizer = {
  #           enable = true;
  #           setupOpts.filetypes = {
  #             "*" = {
  #               RRGGBB = true;
  #               RRGGBBAA = true;
  #             };
  #           };
  #         };
  #         illuminate.enable = true;
  #         breadcrumbs = {
  #           enable = true;
  #           navbuddy.enable = true;
  #         };
  #         smartcolumn = {
  #           enable = true;
  #           setupOpts.custom_colorcolumn = {
  #             nix = "110";
  #             go = [
  #               "90"
  #               "130"
  #             ];
  #           };
  #         };
  #         fastaction.enable = true;
  #       };
  #       utility = {
  #         diffview-nvim.enable = true;
  #         surround.enable = true;
  #
  #         motion = {
  #           hop.enable = true;
  #           leap.enable = true;
  #           precognition.enable = false;
  #         };
  #       };
  #       visuals = {
  #         nvim-scrollbar.enable = false;
  #         nvim-web-devicons = {
  #           enable = true;
  #           setupOpts = { };
  #         };
  #         nvim-cursorline.enable = true;
  #         cinnamon-nvim.enable = true;
  #         fidget-nvim.enable = true;
  #       };
  #     };
  #   };
  #
  stylix.targets.nixvim = {
    enable = true;
    plugin = "mini.base16";
    transparentBackground.main = true;
  };
}

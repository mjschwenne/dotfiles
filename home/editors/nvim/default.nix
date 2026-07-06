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
        superhtml.enable = true;
        cssls.enable = true;
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
        hardtime.enable = false;
        guess-indent.enable = true;
        lz-n.enable = true;
        numbertoggle.enable = true;
        nix.enable = true;
        nix-develop.enable = true;
        friendly-snippets.enable = true;
        csvview.enable = true;
        ts-autotag.enable = true;
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

  stylix.targets.nixvim = {
    enable = true;
    plugin = "mini.base16";
    transparentBackground.main = true;
  };
}

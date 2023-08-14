{ config
, pkgs
, ...
} @ inputs: {
  # User based config
  imports = [ inputs.nixvim.homeManagerModules.nixvim ];

  # System based config
  # imports = [ inputs.nixvim.nixosModules.nixvim ];

  # Should use user based, but all of the flake files are system owned...

  programs.nixvim = {
    enable = true;

    globals.mapleader = " ";
    globals.maplocalleader = "m";

    options = {
      backup = false;
      clipboard = "unnamedplus";
      cmdheight = 1;
      completeopt = [ "menuone" "noselect" ];
      conceallevel = 0;
      fileencoding = "utf-8";
      hlsearch = true;
      ignorecase = true;
      pumheight = 10;
      smartcase = true;
      smartindent = true;
      autoindent = true;
      splitbelow = true;
      splitright = true;
      timeoutlen = 500;
      tabstop = 4;
      shiftwidth = 4;
      history = 50;
      autochdir = true;
      guicursor = "i:block";
      number = true;
      relativenumber = true;
      numberwidth = 4;
      signcolumn = "number";
      scrolloff = 8;
      sidescrolloff = 8;
      wrap = false;
      showmode = false;
    };

    colorschemes.catppuccin = {
      enable = true;
      transparentBackground = false;
      flavour = "mocha";
      showBufferEnd = true;
    };

    plugins = {
      lualine = {
        enable = true;
        disabledFiletypes.statusline = [ "dashboard" "NvimTree" "Outline" ];
        sections = {
          lualine_a = [ "mode" ];
          lualine_b = [
            {
              name = "diagnostics";
              extraConfig = {
                source = [ "nvim_diagnostic" ];
                sections = [ "error" "warn" ];
                symbols = {
                  error = " ";
                  warn = " ";
                };
                colored = false;
                update_in_insert = false;
                always_visible = true;
              };
            }
          ];
          lualine_c = [ "filename" "diff" ];
          lualine_x = [ "encoding" "filetype" ];
          lualine_y = [ "location" ];
          lualine_z = [ "progress" ];
        };
      };

      nvim-autopairs = {
        enable = true;
        # TODO Enable true cmp integration
      };

      alpha = {
        enable = true;
        iconsEnabled = false;
        layout = [
          {
            type = "padding";
            val = 2;
          }
          {
            type = "text";
            val =
              inputs.lib.splitString "\n"
                (builtins.readFile ./neovim-header.txt);
            opts = {
              position = "center";
              hl = "Type";
            };
          }
          {
            type = "padding";
            val = 2;
          }
          {
            type = "group";
            val = [
              {
                shortcut = "f";
                desc = "  Find file";
                command = ":Telescope find_files <CR>";
              }
              {
                shortcut = "e";
                desc = "  New file";
                command = ":ene <BAR> startinsert <CR>";
              }
              {
                shortcut = "r";
                desc = "  Recent files";
                command = ":Telescope oldfiles <CR>";
              }
              {
                shortcut = "t";
                desc = "󰉿  Find text";
                command = ":Telescope live_grep <CR>";
              }
              {
                shortcut = "c";
                desc = "  Configuration";
                command = ":e /etc/nixos/home/editors/neovim.nix <CR>";
              }
              {
                shortcut = "q";
                desc = "  Quit Neovim";
                command = ":qa<CR>";
              }
            ];
          }
          {
            type = "padding";
            val = 2;
          }
          {
            type = "text";
            val = " mjschwenne";
            opts = {
              position = "center";
              hl = "Keyword";
            };
          }
        ];
      };

      which-key.enable = true;

      comment-nvim = {
        enable = true;
        padding = true;
        sticky = true;
      };

      nvim-colorizer.enable = true;

      nvim-tree = {
        enable = true;
        openOnSetup = true;
        syncRootWithCwd = true;
        modified.enable = true;
      };

      lsp = {
        enable = true;
        servers = {
          clangd.enable = true;
          rnix-lsp.enable = true;
          pyright.enable = true;
          rust-analyzer.enable = true;
        };
        keymaps.diagnostic = {
          "<leader><localleader>i" = {
            action = "open_float";
            desc = "Open Diagnostic";
          };
          "<leader><localleader>n" = {
            action = "goto_next";
            desc = "Next Diagnostic";
          };
          "<leader><localleader>p" = {
            action = "goto_prev";
            desc = "Previous Diagnostic";
          };
          "[d" = {
            action = "goto_prev";
            desc = "Next Diagnostic";
          };
          "]d" = {
            action = "goto_next";
            desc = "Previous Diagnostic";
          };
        };
        keymaps.lspBuf = {
          "<leader><localleader>d" = {
            action = "declaration";
            desc = "Goto Declaration";
          };
          "<leader><localleader>D" = {
            action = "definition";
            desc = "Goto Definition";
          };
          "<leader><localleader>f" = {
            action = "format";
            desc = "Format";
          };
          "<leader><localleader>h" = {
            action = "hover";
            desc = "Show Context";
          };
          "<leader><localleader>H" = {
            action = "signature_help";
            desc = "Signature Help";
          };
          "<leader><localleader>I" = {
            action = "Implementation";
            desc = "Goto Implementation";
          };
          "<leader><localleader>r" = {
            action = "rename";
            desc = "Rename";
          };
          "<leader><localleader>R" = {
            action = "references";
            desc = "References";
          };
        };
      };

      lsp-lines.enable = true;

      luasnip = { enable = true; };

      nvim-cmp = {
        enable = true;
        mapping = {
          "<CR>" = "cmp.mapping.confirm({ select = true })";
          "<C-j>" = "cmp.mapping.select_next_item()";
          "<C-k>" = "cmp.mapping.select_prev_item()";
          "<Tab>" = {
            modes = [ "i" "s" ];
            action = ''
              function(fallback)
                if cmp.visible() then
                  cmp.select_next_item()
                else
                  fallback()
                end
              end
            '';
          };
          "<S-Tab>" = {
            modes = [ "i" "s" ];
            action = ''
              function(fallback)
                if cmp.visible() then
                  cmp.select_prev_item()
                else
                  fallback()
                end
              end
            '';
          };
        };
        snippet.expand = "luasnip";
        completion.autocomplete = [ "TextChanged" ];
        sources = [
          { name = "nvim_lsp"; }
          { name = "luasnip"; }
          { name = "buffer"; }
          { name = "path"; }
        ];
        formatting = {
          fields = [ "kind" "abbr" "menu" ];
          format = ''
            function (entry, vim_item)
              vim_item.kind = string.format("%s",
                ({
                  Text = "󰉿",
                  Method = "󰆧",
                  Function = "󰊕",
                  Constructor = "",
                  Field = "",
                  Variable = "󰀫",
                  Class = "󰠱",
                  Interface = "",
                  Module = "",
                  Property = "󰜢",
                  Unit = "󰑭",
                  Value = "󰎠",
                  Enum = "",
                  Keyword = "󰌋",
                  Snippet = "",
                  Color = "󰏘",
                  File = "󰈙",
                  Reference = "",
                  Folder = "󰉋",
                  EnumMember = "",
                  Constant = "󰏿",
                  Struct = "",
                  Event = "",
                  Operator = "󰆕",
                  TypeParameter = "",
                  Misc = "",
                })[vim_item.kind]
              )
              vim_item.menu = ({
                nvim_lsp = "[LSP]",
                nvim_lua = "[LSP]",
                luasnip = "[SNIP]",
                buffer = "[BUF]",
                path = "[PATH]",
              })[entry.source.name]
              return vim_item
            end
          '';
        };
      };

      null-ls = {
        enable = true;
        sources = {
          diagnostics = {
            cppcheck.enable = true;
            deadnix.enable = true;
          };
          formatting = {
            alejandra.enable = true;
            black.enable = true;
            jq.enable = true;
            markdownlint.enable = true;
            nixfmt.enable = true;
            nixpkgs_fmt.enable = true;
          };
        };
      };

      treesitter = {
        enable = true;
        ensureInstalled = [
          "bash"
          "bibtex"
          "c"
          "commonlisp"
          "cpp"
          "css"
          "fish"
          "java"
          "latex"
          "ledger"
          "lua"
          "make"
          "markdown"
          "markdown_inline"
          "nix"
          "python"
          "r"
          "regex"
          "rst"
          "rust"
          "sxhkdrc"
          "vim"
          "yuck"
        ];
        indent = true;
        folding = false;
      };

      ts-context-commentstring.enable = true;

      vimtex.enable = true;
      ledger.enable = true;
      nix.enable = true;
      rust-tools.enable = true;
    };

    extraPlugins = with pkgs.vimPlugins; [ friendly-snippets cmp_luasnip ];

    maps = {
      normalVisualOp.";" = ":";

      normal = {
        "<C-h>" = {
          action = "<C-w>h";
          desc = "Move Focus Left";
        };
        "<C-j>" = {
          action = "<C-w>j";
          desc = "Move Focus Down";
        };
        "<C-k>" = {
          action = "<C-w>k";
          desc = "Move Focus Up";
        };
        "<C-l>" = {
          action = "<C-w>l";
          desc = "Move Focus Right";
        };
        "<C-Up>" = {
          action = "<cmd>resize +2<CR>";
          desc = "Heighten Split";
        };
        "<C-Down>" = {
          action = "<cmd>resize -2<CR>";
          desc = "Shorten Split";
        };
        "<C-Left>" = {
          action = "<cmd>vertical resize -2<CR>";
          desc = "Narrow Split";
        };
        "<C-Right>" = {
          action = "<cmd>vertical resize +2<CR>";
          desc = "Widen Split";
        };

        "<leader>" = { desc = "Leader"; };
        "<leader>w" = {
          action = "<cmd>w!<CR>";
          desc = "Save";
        };
        "<leader>q" = {
          action = "<cmd>q!<CR>";
          desc = "Quit";
        };
        "<leader>h" = {
          action = "<cmd>noh<CR>";
          desc = "Remove Highlight";
        };
        "<leader>a" = {
          action = "<cmd>Alpha<CR>";
          desc = "Return to Dashboard";
        };
        "<leader>b" = { desc = "Buffer"; };
        "<leader>bb" = {
          action = "<cmd>Telescope buffers theme=dropdown<CR>";
          desc = "Search Buffers";
        };
        "<leader>bn" = {
          action = "<cmd>bnext<CR>";
          desc = "Next Buffer";
        };
        "<leader>bp" = {
          action = "<cmd>bprevious<CR>";
          desc = "Pervious Buffer";
        };
        "<leader>bs" = {
          action = "<cmd>w<CR>";
          desc = "Save Buffer";
        };
        "<leader>bd" = {
          action = "<cmd>bdelete<CR>";
          desc = "Close Buffer";
        };
        "<leader>bk" = {
          action = "<cmd>bdelete<CR>";
          desc = "Close Buffer";
        };
        "<leader>f" = { desc = "Files"; };
        "<leader>ff" = {
          action = "<cmd>Telescope find_files<CR>";
          desc = "Find File";
        };
        "<leader>fr" = {
          action = "<cmd>Telescope oldfiles<CR>";
          desc = "Find Recent File";
        };
        "<leader>ft" = {
          action = "<cmd>NvimTreeToggle<CR>";
          desc = "Toggle File Tree";
        };
        "<leader><localleader>" = { desc = "Local"; };
        "<leader><localleader>c" = {
          action = "<cmd>LspInfo<CR>";
          desc = "Info";
        };
        "<leader><localleader>C" = {
          action = "<cmd>Mason<CR>";
          desc = "LSP Manager";
        };
        "<leader>s" = { desc = "Search"; };
        "<leader>sb" = {
          action = "<cmd>Telescope buffers theme=dropdown<CR>";
          desc = "Search Buffer";
        };
        "<leader>sC" = {
          action = "<cmd>Telescope colorcheme<CR>";
          desc = "Colorscheme";
        };
        "<leader>sh" = {
          action = "<cmd>Telescope help_tags<CR>";
          desc = "Find Help";
        };
        "<leader>sm" = {
          action = "<cmd>Telescope find_files<CR>";
          desc = "Man Pages";
        };
        "<leader>sf" = {
          action = "<cmd>Telescope man_pages<CR>";
          desc = "Files";
        };
        "<leader>sr" = {
          action = "<cmd>Telescope find_files<CR>";
          desc = "Recent Files";
        };
        "<leader>sR" = {
          action = "<cmd>Telescope oldfiles<CR>";
          desc = "Registers";
        };
        "<leader>sk" = {
          action = "<cmd>Telescope registers<CR>";
          desc = "Keymaps";
        };
        "<leader>sc" = {
          action = "<cmd>Telescope commands<CR>";
          desc = "Commands";
        };
        "<leader>st" = {
          action = "<cmd>Telescope live_grep theme=ivy<CR>";
          desc = "Text";
        };
      };

      visual = {
        "<" = {
          action = "<gv";
          desc = "Un-Indent";
        };
        ">" = {
          action = ">gv";
          desc = "Indent";
        };
        "/" = {
          action = "";
          desc = "Toggle Comment";
        };

        # Put in visual mode doesn't override the clipboard
        "p" = {
          action = ''"_dP'';
          desc = "Put";
        };
      };

      insert."jk" = {
        action = "<ESC>";
        desc = "Return to Normal Mode";
      };
    };
  };
}

{ pkgs, ... } @ inputs: {
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
          lualine_x = [ "encoding" ];
          lualine_y = [ "filetype" ];
          lualine_z = [ "progress" "location" ];
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

      which-key = {
        enable = true;
        registrations = {
          "<leader>b" = "Buffer";
          "<leader>f" = "File";
          "<leader><localleader>" = "Local";
          "<leader>s" = "Search";
        };
      };

      comment-nvim = {
        enable = true;
        padding = true;
        sticky = true;
      };

      nvim-colorizer.enable = true;
      telescope.enable = true;

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
          nixd.enable = true;
          pyright.enable = true;
          rust-analyzer.enable = true;
          java-language-server.enable = true;
          julials.enable = true;
          ltex.enable = true;
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
            action = "implementation";
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
          "julia"
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

    keymaps = [
      {
        key = ";";
        action = ":";
      }

      {
        key = "jk";
        action = "<ESC>";
        mode = "i";
      }

      {
        key = "<";
        action = "<gv";
        mode = "v";
        options.desc = "Un-Indent";
      }
      {
        key = ">";
        action = ">gv";
        mode = "v";
        options.desc = "Indent";
      }
      # {
      #   key = "/";
      #   action = ""; # TODO
      #   mode = "v";
      #   options.desc = "Toggle Comment";
      # }
      {
        key = "p";
        action = ''"_dP'';
        mode = "v";
        options.desc = "Put";
      }

      {
        key = "<C-h>";
        action = "<C-w>h";
        mode = "n";
        options.desc = "Move Focus Left";
      }
      {
        key = "<C-j>";
        action = "<C-w>j";
        mode = "n";
        options.desc = "Move Focus Down";
      }
      {
        key = "<C-k>";
        action = "<C-w>k";
        mode = "n";
        options.desc = "Move Focus Up";
      }
      {
        key = "<C-l>";
        action = "<C-w>l";
        mode = "n";
        options.desc = "Move Focus Right";
      }
      {
        key = "<C-Up>";
        action = "<cmd>resize +2<CR>";
        mode = "n";
        options.desc = "Heighten Split";
      }
      {
        key = "<C-Down>";
        action = "<cmd>resize -2<CR>";
        mode = "n";
        options.desc = "Shorten Split";
      }
      {
        key = "<C-Left>";
        action = "<cmd>vertical resize -2<CR>";
        mode = "n";
        options.desc = "Narrow Split";
      }
      {
        key = "<C-Right>";
        action = "<cmd>vertical resize +2<CR>";
        mode = "n";
        options.desc = "Widen Split";
      }

      {
        key = "<leader>w";
        action = "<cmd>w!<CR>";
        mode = "n";
        options.desc = "Save";
      }
      {
        key = "<leader>q";
        action = "<cmd>q!<CR>";
        mode = "n";
        options.desc = "Quit";
      }
      {
        key = "<leader>h";
        action = "<cmd>noh<CR>";
        mode = "n";
        options.desc = "Remove Highlight";
      }
      {
        key = "<leader>a";
        action = "<cmd>Alpha<CR>";
        mode = "n";
        options.desc = "Dashboard";
      }
      {
        key = "<leader>bb";
        action = "<cmd>Telescope buffers theme=dropdown<CR>";
        mode = "n";
        options.desc = "Search Buffers";
      }
      {
        key = "<leader>bn";
        action = "<cmd>bnext<CR>";
        mode = "n";
        options.desc = "Next Buffer";
      }
      {
        key = "<leader>bp";
        action = "<cmd>bprevious<CR>";
        mode = "n";
        options.desc = "Pervious Buffer";
      }
      {
        key = "<leader>bs";
        action = "<cmd>w<CR>";
        mode = "n";
        options.desc = "Save buffer";
      }
      {
        key = "<leader>bd";
        action = "<cmd>bdelete<CR>";
        mode = "n";
        options.desc = "Delete Buffer";
      }
      {
        key = "<leader>bk";
        action = "<cmd>bdelete<CR>";
        mode = "n";
        options.desc = "Kill Buffer";
      }
      {
        key = "<leader>ff";
        action = "<cmd>Telescope find_files<CR>";
        mode = "n";
        options.desc = "Find File";
      }
      {
        key = "<leader>fr";
        action = "<cmd>Telescope oldfiles<CR>";
        mode = "n";
        options.desc = "Find Recent File";
      }
      {
        key = "<leader>ft";
        action = "<cmd>NvimTreeToggle<CR>";
        mode = "n";
        options.desc = "Toggle File Tree";
      }
      {
        key = "<leader><localleader>m";
        action = "<cmd>LspInfo<CR>";
        mode = "n";
        options.desc = "LSP Info";
      }
      {
        key = "<leader><localleader>M";
        action = "<cmd>Mason<CR>";
        mode = "n";
        options.desc = "LSP Manager";
      }
      {
        key = "<leader>sb";
        action = "<cmd>Telescope buffers theme=dropdown<CR>";
        mode = "n";
        options.desc = "Search Buffers";
      }
      {
        key = "<leader>sc";
        action = "<cmd>Telescope commands<CR>";
        mode = "n";
        options.desc = "Search Commands";
      }
      {
        key = "<leader>sC";
        action = "<cmd>Telescope colorscheme<CR>";
        mode = "n";
        options.desc = "Search Colorschemes";
      }
      {
        key = "<leader>sh";
        action = "<cmd>Telescope help_tags<CR>";
        mode = "n";
        options.desc = "Search Help";
      }
      {
        key = "<leader>sf";
        action = "<cmd>Telescope find_files<CR>";
        mode = "n";
        options.desc = "Search Files";
      }
      {
        key = "<leader>sm";
        action = "<cmd>Telescope man_pages<CR>";
        mode = "n";
        options.desc = "Search Man Pages";
      }
      {
        key = "<leader>sr";
        action = "<cmd>Telescope oldfiles<CR>";
        mode = "n";
        options.desc = "Search Recent Files";
      }
      {
        key = "<leader>sR";
        action = "<cmd>Telescope registers<CR>";
        mode = "n";
        options.desc = "Search Registers";
      }
      {
        key = "<leader>sk";
        action = "<cmd>Telescope keymaps<CR>";
        mode = "n";
        options.desc = "Search Keymaps";
      }
      {
        key = "<leader>st";
        action = "<cmd>Telescope live_grep theme=ivy<CR>";
        mode = "n";
        options.desc = "Search Text";
      }
    ];

    extraConfigLua = ''
    -- Highlight on Yank
    vim.cmd([[
	        augroup highlight_yank
	        autocmd!
	        au TextYankPost * silent! lua vim.highlight.on_yank({higroup="Visual", timeout=200})
	        augroup END
    ]])
    '';
  };
}

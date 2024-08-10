-- if true then return {} end -- WARN: REMOVE THIS LINE TO ACTIVATE THIS FILE

-- AstroCore provides a central place to modify mappings, vim options, autocommands, and more!
-- Configuration documentation can be found with `:h astrocore`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- Configure core features of AstroNvim
    features = {
      large_buf = { size = 1024 * 500, lines = 10000 }, -- set global limits for large files for disabling features like treesitter
      autopairs = true, -- enable autopairs at start
      cmp = true, -- enable completion at start
      diagnostics_mode = 3, -- diagnostic mode on start (0 = off, 1 = no signs/virtual text, 2 = no virtual text, 3 = on)
      highlighturl = true, -- highlight URLs at start
      notifications = true, -- enable notifications at start
    },
    -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
    diagnostics = {
      virtual_text = true,
      underline = true,
    },
    -- vim options can be configured here
    options = {
      opt = { -- vim.opt.<key>
        spell = false, -- sets vim.opt.spell
        backup = false, -- Do not create a ~ backup file
        clipboard = "unnamedplus", -- Use the '+' regiester for clipboard
        fileencoding = "utf-8", -- Write all files in utf-8
        -- foldmethod = "marker", -- Enable folding between {{{ }}} blocks
        hlsearch = true, -- Highlight all matches during a search
        ignorecase = true, -- Ignore case while searaching
        pumheight = 10, -- Height of the popup menu
        smartcase = true, -- Override ignorecase if search has captial letter
        smartindent = true, -- Smart new line indentation
        autoindent = true, -- Recommended with smartindent
        splitbelow = true, -- Horitzontal splits always appear below
        splitright = true, -- Vertical splits always appear right
        timeoutlen = 500, -- Timeout for command key sequences
        tabstop = 4, -- Tab width
        shiftwidth = 4, -- Tab width if replaced by spaces
        history = 50, -- Number of previous commands and searchs remembered
        autochdir = true, -- Keep the cwd to the directory of the active buffer
        number = true, -- Line numbers
        relativenumber = true, -- Relative line numbers
        numberwidth = 4, -- Number of characters for the line numbers
        signcolumn = "yes:1", -- How to display the sign column
        scrolloff = 8, -- Number of lines above/below scrolling cursor
        sidescrolloff = 8, -- Number of character left/right scrolling cursor
        wrap = false, -- Do not wrap lines
        showmode = false, -- Do not show mode in command gutter
      },
      g = { -- vim.g.<key>
        -- configure global vim variables (vim.g)
        -- NOTE: `mapleader` and `maplocalleader` must be set in the AstroNvim opts or before `lazy.setup`
        -- This can be found in the `lua/lazy_setup.lua` file
      },
    },
    -- Mappings can be configured through AstroCore as well.
    -- NOTE: keycodes follow the casing in the vimdocs. For example, `<Leader>` must be capitalized
    mappings = {
      -- first key is the mode
      n = {
        -- second key is the lefthand side of the map

        -- navigate buffer tabs with `H` and `L`
        L = { function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
        H = { function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },

        -- mappings seen under group name "Buffer"
        ["<Leader>bD"] = {
          function()
            require("astroui.status.heirline").buffer_picker(
              function(bufnr) require("astrocore.buffer").close(bufnr) end
            )
          end,
          desc = "Pick to close",
        },
        -- tables with just a `desc` key will be registered with which-key if it's installed
        -- this is useful for naming menus
        ["<Leader>b"] = { desc = "Buffers" },
        ["<leader>bd"] = { ":bd<cr>", desc = "Delete buffer" },
        -- quick save
        -- ["<C-s>"] = { ":w!<cr>", desc = "Save File" },  -- change description but the same command
      },
      t = {
        -- setting a mapping to false will disable it
        -- ["<esc>"] = false,
      },
      v = {
        ["<"] = { "<gv", desc = "Indent" },
        [">"] = { ">gv", desc = "Un-Indent" },
      },
    },
  },
}

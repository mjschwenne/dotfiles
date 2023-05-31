local map = vim.api.nvim_set_keymap -- Shorten the function name

-- Remap the leader key to be the space bar
map("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = "m"

-- Every keymap must use WhichKey
local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
	print("Failed to load which-key...")
	return
end

local setup = {
	plugins = {
		marks = true, -- shows a list of your marks on ' and `
		registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
		spelling = {
			enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
			suggestions = 20, -- how many suggestions should be shown in the list?
		},
		-- the presets plugin, adds help for a bunch of default keybindings in Neovim
		-- No actual key bindings are created
		presets = {
			operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
			motions = true, -- adds help for motions
			text_objects = true, -- help for text objects triggered after entering an operator
			windows = true, -- default bindings on <c-w>
			nav = true, -- misc bindings to work with windows
			z = true, -- bindings for folds, spelling and others prefixed with z
			g = true, -- bindings for prefixed with g
		},
	},
	-- add operators that will trigger motion and text object completion
	-- to enable all native operators, set the preset / operators plugin above
	-- operators = { gc = "Comments" },
	key_labels = {
		-- override the label used to display some keys. It doesn't effect WK in any other way.
		["<space>"] = "SPC",
		["<CR>"] = "RET",
		["<tab>"] = "TAB",
	},
	icons = {
		breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
		separator = "➜", -- symbol used between a key and it's label
		group = "+", -- symbol prepended to a group
	},
	popup_mappings = {
		scroll_down = "<c-d>", -- binding to scroll down inside the popup
		scroll_up = "<c-u>", -- binding to scroll up inside the popup
	},
	window = {
		border = "rounded", -- none, single, double, shadow
		position = "bottom", -- bottom, top
		margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
		padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
		winblend = 0,
	},
	layout = {
		height = { min = 4, max = 25 }, -- min and max height of the columns
		width = { min = 20, max = 50 }, -- min and max width of the columns
		spacing = 3, -- spacing between columns
		align = "left", -- align columns left, center or right
	},
	ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
	hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
	show_help = true, -- show help message on the command line when the popup is visible
	triggers = "auto", -- automatically setup triggers
	-- triggers = {"<leader>"} -- or specify a list manually
	triggers_blacklist = {
		-- list of mode / prefixes that should never be hooked by WhichKey
		-- this is mostly relevant for key maps that start with a native binding
		-- most people should not need to change this
		i = { "j", "k" },
		v = { "j", "k" },
	},
}
which_key.setup(setup)

-- Modes
-- 	normal mode       -> "n"
-- 	insert mode       -> "i"
-- 	visual mode       -> "v"
-- 	visual block mode -> "x"
-- 	terminal mode     -> "t"
-- 	command mode      -> "c"

-- Normal Mode --
local n_mappings = {
	-- Window Managment
	["<C-h>"] = { "<C-w>h", "Move Focus Left" },
	["<C-j>"] = { "<C-w>j", "Move Focus Down" },
	["<C-k>"] = { "<C-w>k", "Move Focus Up" },
	["<C-l>"] = { "<C-w>l", "Move Focus Right" },

	["<C-Up>"] = { "<cmd>resize +2<CR>", "Heighten Split" },
	["<C-Down>"] = { "<cmd>resize -2<CR>", "Shorten Split" },
	["<C-Left>"] = { "<cmd>vertical resize -2<CR>", "Narrow Split" },
	["<C-Right>"] = { "<cmd>vertical resize +2<CR>", "Widen Split" },

	-- Leader Based Keymaps
	["<leader>"] = {
		-- General Shortcuts
		w = { "<cmd>w!<CR>", "Save" },
		q = { "<cmd>q!<CR>", "Quit" },
		h = { "<cmd>noh<CR>", "No Highlight" },
		a = { "<cmd>Alpha<CR>", "Return to Dashboard" },

		-- Buffer Management
		b = {
			name = "Buffer",

			b = { "<cmd>Telescope buffers theme=dropdown<CR>", "Search Buffer" },
			n = { "<cmd>bnext<CR>", "Next Buffer" },
			p = { "<cmd>bprevious<CR>", "Pervious Buffer" },
			s = { "<cmd>w<CR>", "Save Buffer" }, -- parity with my emacs bindings
			d = { "<cmd>bdelete<CR>", "Close Buffer" },
			k = { "<cmd>bdelete<CR>", "Close Buffer" },
		},

		-- Files
		f = {
			name = "Files",

			f = { "<cmd>Telescope find_files<CR>", "Find File" },
			r = { "<cmd>Telescope oldfiles<CR>", "Find Recent File" },
			e = { "<cmd>NvimTreeToggle<CR>", "Toggle File Explorer" },
		},

		-- Nvim Plugin Management
		p = {
			name = "Packer",

			c = { "<cmd>PackerCompile<CR>", "Compile" },
			i = { "<cmd>PackerInstall<CR>", "Install" },
			s = { "<cmd>PackerSync<CR>", "Sync" },
			S = { "<cmd>PackerStatus<CR>", "Status" },
			u = { "<cmd>PackerUpdate<CR>", "Update" },
		},

		-- LSP related
		["<localleader>"] = {
			name = "Local",

			i = { "<cmd>LspInfo<CR>", "Info" },
			I = { "<cmd>Mason<CR>", "LSP Installer" },
		},

		-- Telescope Based Searches
		s = {
			name = "Search",

			b = { "<cmd>Telescope buffers theme=dropdown<CR>", "Search Buffer" },
			C = { "<cmd>Telescope colorscheme<CR>", "Colorscheme" },
			h = { "<cmd>Telescope help_tags<CR>", "Find Help" },
			M = { "<cmd>Telescope man_pages<CR>", "Man Pages" },
			f = { "<cmd>Telescope find_files<CR>", "Files" },
			r = { "<cmd>Telescope oldfiles<CR>", "Recent Files" },
			R = { "<cmd>Telescope registers<CR>", "Registers" },
			k = { "<cmd>Telescope keymaps<CR>", "Keymaps" },
			c = { "<cmd>Telescope commands<CR>", "Commands" },
			t = { "<cmd>Telescope live_grep theme=ivy<CR>", "Text" },
		},
	},
}

which_key.register(n_mappings)

-- Visual Mode --
local v_mappings = {
	-- Stay in indent mode
	["<"] = { "<gv", "Indent" },
	[">"] = { ">gv", "Un-Indent" },
	["/"] = { '<Plug>(comment_toggle_linewise_visual)', "Comment" },

	-- Put in visual mode doesn't override the clipboard
	p = { '"_dP', "Put" },
}

which_key.register(v_mappings, { mode = "v" })

-- Visual Block Mode --
local x_mappings = {
	-- Move text up and down --
	J = { "<cmd>move '>+1<CR>gv-gv<CR>", "Move Block Up" },
	K = { "<cmd>move '<-2<CR>gv-gv<CR>", "Move Block Down" },

	["<A>"] = {
		j = { "<cmd>move '>+1<CR>gv-gv<CR>", "Move Block Up" },
		k = { "<cmd>move '<-2<CR>gv-gv<CR>", "Move Block Down" },
	},
}

which_key.register(x_mappings, { mode = "x" })

-- Insert Mode --
local i_mappings = {
	jk = {"<ESC>", "Return to Normal Mode"}
}

which_key.register(i_mappings, { mode = "i" })

-- Terminal Mode --
local t_mappings = {
	-- Align with split navigation
	["<C>"] = {
		h = { "<C-\\><C-N><C-w>h", "Move Focus Left" },
		j = { "<C-\\><C-N><C-w>j", "Move Focus Down" },
		k = { "<C-\\><C-N><C-w>k", "Move Focus Up" },
		l = { "<C-\\><C-N><C-w>l", "Move Focus Right" },
	},
}

which_key.register(t_mappings, { mode = "t" })

local catppuccin_p, catppuccin = pcall(require, "catppuccin")
if catppuccin_p then
	catppuccin.setup({
		flavour = "mocha",
		show_end_of_buffer = true,
		integrations = {
			alpha = true,
			gitsigns = true,
			mini = {
				enabled = true,
				indentscope_color = "",
			},
			nvimtree = true,
			treesitter = true,
		}
	})

	vim.cmd.colorscheme "catppuccin"
else
	print "Failed to load catppuccin..."
end


local alpha_p, alpha = pcall(require, "alpha")
if alpha_p then
	local dashboard = require("alpha.themes.dashboard")
	dashboard.section.header.val = {
		[[         _             _            _      _          _        _         _   _      ]],
		[[        /\ \     _    /\ \         /\ \   /\ \    _ / /\      /\ \      /\_\/\_\ _  ]],
		[[       /  \ \   /\_\ /  \ \       /  \ \  \ \ \  /_/ / /      \ \ \    / / / / //\_\]],
		[[      / /\ \ \_/ / // /\ \ \     / /\ \ \  \ \ \ \___\/       /\ \_\  /\ \/ \ \/ / /]],
		[[     / / /\ \___/ // / /\ \_\   / / /\ \ \ / / /  \ \ \      / /\/_/ /  \____\__/ / ]],
		[[    / / /  \/____// /_/_ \/_/  / / /  \ \_\\ \ \   \_\ \    / / /   / /\/________/  ]],
		[[   / / /    / / // /____/\    / / /   / / / \ \ \  / / /   / / /   / / /\/_// / /   ]],
		[[  / / /    / / // /\____\/   / / /   / / /   \ \ \/ / /   / / /   / / /    / / /    ]],
		[[ / / /    / / // / /______  / / /___/ / /     \ \ \/ /___/ / /__ / / /    / / /     ]],
		[[/ / /    / / // / /_______\/ / /____\/ /       \ \  //\__\/_/___\\/_/    / / /      ]],
		[[\/_/     \/_/ \/__________/\/_________/         \_\/ \/_________/        \/_/       ]],
	}

	dashboard.section.buttons.val = {
		dashboard.button("f", "  Find file", ":Telescope find_files <CR>"),
		dashboard.button("e", "  New file", ":ene <BAR> startinsert <CR>"),
		dashboard.button("r", "  Recently used files", ":Telescope oldfiles <CR>"),
		dashboard.button("t", "󰉿  Find text", ":Telescope live_grep <CR>"),
		dashboard.button("c", "  Configuration", ":e ~/.config/nvim/init.lua <CR>"),
		dashboard.button("q", "󰩈  Quit Neovim", ":qa<CR>"),
	}

	local function footer()
		local fortune_ok, fortune = pcall(require, "alpha.fortune")
		if not fortune_ok then
			return "  mjschwenne"
		end
		return fortune()
	end

	dashboard.section.footer.val = footer()

	dashboard.section.footer.opts.hl = "Type"
	dashboard.section.header.opts.hl = "Include"
	dashboard.section.buttons.opts.hl = "Keyword"

	dashboard.opts.opts.noautocmd = true
	alpha.setup(dashboard.opts)
else
	print "Failed to load alpha..."
end

local lualine_p, lualine = pcall(require, "lualine")
if lualine_p then
	vim.api.nvim_set_hl(0, "MjsLualineUnmodified", {fg = "#a6e3a1"})
	vim.api.nvim_set_hl(0, "MjsLualineModified", {fg = "#f38ba8"})
	vim.api.nvim_set_hl(0, "MjsLualineReadonly", {fg = "#f9e2af"})

	lualine.setup({
		options = {
			disabled_filetypes = { "dashboard", "NvimTree", "Outline" },
			component_separators = { left = "", right = "" },
			always_divide_middle = true,
			globalstatus = true,
		},
		sections = {
			lualine_a = { "mode" },
			lualine_b = {
				{
					"diagnostics",
					sources = { "nvim_diagnostic", "nvim_lsp" },
					sections = { "error", "warn", "info", "hint" },
					symbols = { error = ' ', warn = ' ', info = ' ', hint = ' ' },
					colored = false,
					update_in_insert = true,
					always_visible = false,
				},
			},
			lualine_c = {
				{
					"filename",
					fmt = function(str)
						local sym = '%#MjsLualineUnmodified#󱣪 '
						if vim.bo.modified then
							sym = '%#MjsLualineModified#󰆓 '
						elseif vim.bo.readonly then
							sym = '%#MjsLualineReadonly#󱙃 '
						end

						return sym .. "%#Default#" .. str
					end,
					file_status = false,
				},
				"lsp_progress",
			},
			lualine_x = { "encoding" },
			lualine_y = { "filetype" },
			lualine_z = { "progress", "location" }
		},
	})
else
	print "Failed to load lualine..."
end

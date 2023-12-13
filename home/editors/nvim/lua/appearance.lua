local catppuccin_p, catppuccin = pcall(require, "catppuccin")
if not catppuccin_p then 
	print "Failed to load catppuccin..."
	return
end

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

local alpha_p, alpha = pcall(require, "alpha")
if not alpha_p then
	print "Failed to load alpha..."
	return
end

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

local feline_p, feline = pcall(require, "feline")
if not feline_p then 
	print "Failed to load feline..."
	return 
end 

feline.setup()

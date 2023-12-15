local coq_p, _ = pcall(require, "coq")
local coq_3p_p, coq_3p = pcall(require, "coq_3p")
if coq_p and coq_3p_p then
	vim.g.coq_settings = {
		keymap = {
			recommended = false,
			jump_to_mark = "<c-s>",
			pre_select = true,
		}
	}

	-- Mostly sane default maps :)
	local remap = vim.api.nvim_set_keymap
	remap('i', '<esc>', [[pumvisible() ? "<c-e><esc>" : "<esc>"]], { expr = true, noremap = true })
	remap('i', '<c-c>', [[pumvisible() ? "<c-e><c-c>" : "<c-c>"]], { expr = true, noremap = true })
	remap('i', '<tab>', [[pumvisible() ? "<c-n>" : "<tab>"]], { expr = true, noremap = true })
	remap('i', '<s-tab>', [[pumvisible() ? "<c-p>" : "<bs>"]], { expr = true, noremap = true })
	remap('i', '<c-j>', [[pumvisible() ? "<c-n>" : "<c-j>"]], { expr = true, noremap = true })
	remap('i', '<c-k>', [[pumvisible() ? "<c-p>" : "<c-k>"]], { expr = true, noremap = true })

	coq_3p {
		{ src = "nvimlua", short_name = "nLUA" },
		{ src = "vimtex", short_name = "vTEX" },
		{ src = "bc", short_name = "MATH", precision = 6},
		{ src = "figlet", short_name = "BIG", trigger = "!big", fonts = { "doom" }},
		{ src = "cow", trigger = "!moo"}
		-- { src = "dap" }
	}
	vim.cmd('COQnow -s')
end

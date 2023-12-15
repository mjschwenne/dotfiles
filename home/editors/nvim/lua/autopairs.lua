local remap = vim.api.nvim_set_keymap
local npairs_p, npairs = pcall(require, "nvim-autopairs")

if npairs_p then
	vim.g.coq_settings = { keymap = { recommended = false } }

	npairs.setup({
		map_bs = false,
		map_cr = false,
		check_ts = true,
		ts_config = {
			lua = { "string", "source" },
			java = true,
		},
		disable_filetype = { "TelescopePrompt" },
		fast_wrap = {
			map = "<M-e>",
			chars = { "{", "[", "(", '"', "'", "`" },
			pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
			offset = 0, -- Offset from pattern match
			end_key = "$",
			keys = "qwertyuiopzxcvbnmasdfghjkl",
			check_comma = true,
			highlight = "PmenuSel",
			highlight_grey = "LineNr",
		},
	})

	-- skip it, if you use another global object
	_G.MUtils= {}

	MUtils.CR = function()
	  if vim.fn.pumvisible() ~= 0 then
		if vim.fn.complete_info({ 'selected' }).selected ~= -1 then
		  return npairs.esc('<c-y>')
		else
		  return npairs.esc('<c-e>') .. npairs.autopairs_cr()
		end
	  else
		return npairs.autopairs_cr()
	  end
	end
	remap('i', '<cr>', 'v:lua.MUtils.CR()', { expr = true, noremap = true })

	MUtils.BS = function()
	  if vim.fn.pumvisible() ~= 0 and vim.fn.complete_info({ 'mode' }).mode == 'eval' then
		return npairs.esc('<c-e>') .. npairs.autopairs_bs()
	  else
		return npairs.autopairs_bs()
	  end
	end
	remap('i', '<bs>', 'v:lua.MUtils.BS()', { expr = true, noremap = true })
else
	print "Failed to load autopairs..."
end




-- following options are the default
-- each of these are documented in `:help nvim-tree.OPTION_NAME`

local nvim_tree_p, nvim_tree = pcall(require, "nvim-tree")
if nvim_tree_p then
	nvim_tree.setup()

	vim.cmd [[autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif]]
else
	print "Failed to load nvim-tree..."
end

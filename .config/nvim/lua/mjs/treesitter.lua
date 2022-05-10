local status_ok, configs = pcall(require, "nvim-treesitter.configs")
if not status_ok then
	print "Failed to load treesitter configs..."
	return
end

configs.setup {
	ensure_installed = {
		"bibtex",
		"c",
		"cooklang",
		"cpp",
		"fish",
		"latex",
		"lua",
		"make",
		"norg",
		"python",
		"r",
		"regex",
		"vim"
	},
	sync_install = false,
	ignore_install = { "" }, 			-- List of parsers to ignore installing
	highlight = {
    	enable = true,	 				-- false will disable the whole extension
    	disable = { "" }, 				-- List of language that will be disabled
    	additional_vim_regex_highlighting = true,
	},
	indent = { enable = true, disable = { "yaml" } },
	context_commentstring = {
		enable = true,
		enable_autocmd = false,
	},
}

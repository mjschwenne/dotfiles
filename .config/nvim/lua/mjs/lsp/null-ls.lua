local status_ok, null_ls = pcall(require, "null-ls")
if not status_ok then
	print "Failed to load null-ls..."
	return
end

local formatting = null_ls.builtins.formatting
-- local diagnostics = null_ls.builtins.diagnostics

null_ls.setup {
	debug = false,
	sources = {
		-- python
		formatting.black,
		-- diagnostics.flake8,
		-- LaTeX
		formatting.latexindent,
		-- lua
		formatting.stylua,
	}
}

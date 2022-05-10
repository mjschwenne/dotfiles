local status_ok, null_ls = pcall(require, "null-ls")
if not status_ok then
	print "Failed to load null-ls..."
	return
end

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagonistics

null_ls.setup {
	debug = false,
	sources = {
		-- python
		formatting.black,
		diagnostics.flake8,
		-- C / C++
		formatting.clang_format.with {extra_args = { "-style='{IndentWidth: 4, BreakBeforeBraces: Allman}" } },
		-- LaTeX
		formatting.latexindent,
		-- lua
		formatting.stylua,
	}
}

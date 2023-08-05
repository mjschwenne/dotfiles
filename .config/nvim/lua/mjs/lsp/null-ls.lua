local status_ok, null_ls = pcall(require, "null-ls")
if not status_ok then
	print("Failed to load null-ls...")
	return
end

local code_actions = null_ls.builtins.code_actions
local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics

null_ls.setup({
	debug = false,
	sources = {
		-- All
		formatting.trim_whitespace,
		formatting.trim_newlines,

		-- C
		formatting.clang_format.with({
			extra_args = {"-i", '-style={"IndentWith:4, BreakBeforeBraces: Allman"}'}
		}),

		-- fish
		diagnostics.fish,
		formatting.fish_indent,

		-- LaTeX
		code_actions.proselint,
		formatting.latexindent,
		formatting.bibclean,
		diagnostics.chktex,

		-- lua
		formatting.stylua,

		-- markdown
		-- diagnostics.write_good,

		-- python
		formatting.black,
		-- diagnostics.flake8,
		formatting.isort,

		-- R
		formatting.format_r,

		-- Rust
		formatting.rustfmt,
	},
})

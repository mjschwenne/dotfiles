local ht_p, ht = pcall(require, "haskell-tools")
if not ht_p then
	print "Failed to load haskell-tools..."
	return
end

local bufnr = vim.api.nvim_get_current_buf()
require("which-key").register({
	['<leader>'] = {
		['<localleader>'] = {
			m = {
				name = "Haskell",
				e = { ht.lsp.buf_eval_all, "Evaluate All Lines" },
				l = { function() ht.repl.toggle(vim.api.nvim_buf_get_name(0)) end, "Buffer REPL" },
				L = { ht.repl.toggle, "Package REPL" },
				q = { ht.repl.quit, "Quite REPL" },
				r = { vim.lsp.codelens.run, "Run Statement" },
				s = { ht.hoogle.hoogle_signature, "Signature Search" },
			}
		}
	}
}, { buffer = bufnr })

vim.o.expandtab = true

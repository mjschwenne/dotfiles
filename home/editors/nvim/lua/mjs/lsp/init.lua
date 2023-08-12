local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
	print "Failed to load lspconfig..."
	return
end

-- Call the relevant LSP functions
require("mjs.lsp.lsp-installer")
require("mjs.lsp.handlers").setup()
require("mjs.lsp.null-ls")

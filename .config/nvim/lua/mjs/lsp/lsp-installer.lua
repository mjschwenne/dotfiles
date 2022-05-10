local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
if not status_ok then
	print "Failed to load lsp installer..."
	return
end

local status_ok, lspconfig = pcall(require, "lspconfig")
if not status_ok then
	print "Failed to load lspconfig..."
	return
end

lsp_installer.setup{}

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances instead (see example below).
for _, server in ipairs(lsp_installer.get_installed_servers()) do
	local opts = {
		on_attach = require("mjs.lsp.handlers").on_attach,
		capabilities = require("mjs.lsp.handlers").capabilities,
	}

	 if server.name == "sumneko_lua" then
	 	local sumneko_opts = require("mjs.lsp.settings.sumneko_lua")
	 	opts = vim.tbl_deep_extend("force", sumneko_opts, opts)
	 end

	 if server.name == "pyright" then
	 	local pyright_opts = require("mjs.lsp.settings.pyright")
	 	opts = vim.tbl_deep_extend("force", pyright_opts, opts)
	 end

	lspconfig[server.name].setup(opts)
end

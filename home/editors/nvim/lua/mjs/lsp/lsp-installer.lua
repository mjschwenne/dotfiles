local status_ok, mason = pcall(require, "mason")
if not status_ok then
	print "Failed to load mason..."
	return
end

local status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not status_ok then 
	print "Failed to load mason-lspconfig bridge..."
	return
end

local status_ok, lspconfig = pcall(require, "lspconfig")
if not status_ok then
	print "Failed to load lspconfig..."
	return
end

mason.setup(
	{ui = {
    	icons = {
     	 	package_installed = "✓",
     		package_pending = "➜",
      		package_uninstalled = "✗"
     	}
  	}
})

mason_lspconfig.setup_handlers {
  	function(server_name)
		local server_settings
		if server_name == "lua_ls" then
			server_settings = require("mjs.lsp.settings.sumneko_lua")
		end

		if server_name == "pyright" then
			server_settings = require("mjs.lsp.settings.pyright")
		end

		if server_name == "clangd" then
			server_settings = require("mjs.lsp.settings.clangd")
		end

    	lspconfig[server_name].setup {
      		capabilities = require("mjs.lsp.handlers").capabilities,
      		on_attach = require("mjs.lsp.handlers").on_attach,
			settings = server_settings,
    	}
 	 end,
}

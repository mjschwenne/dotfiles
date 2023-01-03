local M = {}

M.setup = function()
	local signs = {
		{ name = "DiagnosticSignError", text = "" },
		{ name = "DiagnosticSignWarn", text = "" },
		{ name = "DiagnosticSignHint", text = "" },
		{ name = "DiagnosticSignInfo", text = "" },
	}

	for _, sign in ipairs(signs) do
		vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
	end

  local config = {
    -- disable virtual text
    virtual_text = false,
    -- show signs
	signs = {
		active = signs,
	},
		update_in_insert = true,
		underline = true,
		severity_sort = true,
		float = {
			focusable = false,
			style = "minimal",
			border = "rounded",
			source = "always",
			header = "",
			prefix = "",
		},
	}

	vim.diagnostic.config(config)

	vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
		border = "rounded",
	})

	vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
		border = "rounded",
	})
end

local function lsp_highlight_document(client)
	-- Set autocommands conditional on server_capabilities
	if client.server_capabilities.document_highlight then
		vim.api.nvim_exec(
			[[
				augroup lsp_document_highlight
				autocmd! * <buffer>
				autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
				autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
				augroup END
			]],
			false
		)
	end
end

local function lsp_keymaps(bufnr)
	local status_ok, which_key = pcall(require, "which-key")
	if not status_ok then
		print("Failed to load which-key...")
		return
	end

	which_key.register({
		["<leader><localleader>"] = {
			a = { "<cmd>lua vim.lsp.buf.code_action()<CR>", "Code Action" },
			d = { "<cmd>lua vim.lsp.buf.declaration()<CR>", "Goto Declaration"},
			D = { "<cmd>lua vim.lsp.buf.definition()<CR>", "Goto Definition"},
			f = { "<cmd>lua vim.lsp.buf.format({ async = true })<CR>", "Format" },
			h = { "<cmd>lua vim.lsp.buf.hover()<CR>", "Show Context"},
			H = { "<cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature Help"},
			i = { "<cmd>lua vim.diagnostic.open_float()<CR>", "Open Diagnostic"},
			I = { "<cmd>lua vim.lsp.buf.implementation()<CR>", "Goto Implementation"},
			n = { "<cmd>lua vim.diagnostic.goto_next()<CR>", "Next Diagnostic" },
			p = { "<cmd>lua vim.diagnostic.goto_prev()<CR>", "Previous Diagnostic" },
			r = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename" },
			R = { "<cmd>lua vim.lsp.buf.references()<CR>", "References"},
			s = { "<cmd>Telescope lsp_document_symbols<CR>", "Document Symbols" },
			S = { "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", "Workspace Symbols" },
			w = { "<cmd>Telescope lsp_document_diagnostics<CR>", "Document Diagnostics" },
			W = { "<cmd>Telescope lsp_workspace_diagnostics<CR>", "Workspace Diagnostics" },
			["/"] = { '<Plug>(comment_toggle_linewise)', "Comment" },
		},
		["[d"] = {"<cmd>lua vim.diagnostic.goto_next()<CR>", "Next Diagnostic"},
		["]d"] = {"<cmd>lua vim.diagnostic.goto_prev()<CR>", "Next Diagnostic"}
	}, { mode = "n", buffer = bufnr })

	vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting_sync()' ]]
end

M.on_attach = function(client, bufnr)
	lsp_keymaps(bufnr)
	lsp_highlight_document(client)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()

local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not status_ok then
	print "Failed to load cmp_nvim_lsp..."
	return
end

M.capabilities = cmp_nvim_lsp.default_capabilities(capabilities)

return M

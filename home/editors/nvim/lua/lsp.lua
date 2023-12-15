local lspconfig_p, lspconfig = pcall(require, "lspconfig")
if lspconfig_p then
	lspconfig.clangd.setup {}
	lspconfig.hls.setup {}
	lspconfig.jdtls.setup {}
	lspconfig.ltex.setup {}
	lspconfig.lua_ls.setup {
		on_init = function(client)
			local path = client.workspace_folders[1].name
			if not vim.loop.fs_stat(path .. '/.luarc.json') and not vim.loop.fs_stat(path .. '/.luarc.jsonc') then
				client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
					Lua = {
						runtime = {
							-- Tell the language server which version of Lua you're using
							-- (most likely LuaJIT in the case of Neovim)
							version = 'LuaJIT'
						},
						-- Make the server aware of Neovim runtime files
						workspace = {
							checkThirdParty = false,
							library = {
								vim.env.VIMRUNTIME
								-- "${3rd}/luv/library"
								-- "${3rd}/busted/library",
							}
							-- or pull in all of 'runtimepath'. NOTE: this is a lot slower
							-- library = vim.api.nvim_get_runtime_file("", true)
						}
					}
				})

				client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
			end
			return true
		end
	}
	lspconfig.nixd.setup {}
	lspconfig.pyright.setup {}
	lspconfig.rust_analyzer.setup {}

	whichkey.register({
		["[e"] = { vim.diagnostic.goto_prev, "Previous Diagnostic" },
		["]e"] = { vim.diagnostic.goto_next, "Next Diagnostic" },
		["<leader>"] = {
			i = { vim.diagnostic.open_float, "Diagnostic Information" },
			["<localleader>"] = {
				i = { vim.diagnostic.open_float, "Information" },
			}
		}
	})

	vim.api.nvim_create_autocmd('LspAttach', {
		group = vim.api.nvim_create_augroup('UserLspConfig', {}),
		callback = function(ev)
			-- Enable completion with omnifunc
			vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

			-- Buffer local keymaps
			whichkey.register({
				["<leader><localleader>"] = {
					a = { vim.lsp.buf.code_action, "Code Action" },
					d = { vim.lsp.buf.declaration, "Goto Declaration" },
					D = { vim.lsp.buf.definition, "Goto Definition" },
					f = { function()
						vim.lsp.buf.format({ async = true })
					end, "Format" },
					h = { vim.lsp.buf.hover, "Show Context" },
					H = { vim.lsp.buf.signature_help, "Signature Help" },
					i = { vim.diagnostic.open_float, "Open Diagnostic" },
					I = { vim.lsp.buf.implementation, "Goto Implementation" },
					n = { vim.diagnostic.goto_next, "Next Diagnostic" },
					p = { vim.diagnostic.goto_prev, "Previous Diagnostic" },
					r = { vim.lsp.buf.rename, "Rename" },
					R = { vim.lsp.buf.references, "References" },
					s = { "<cmd>Telescope lsp_document_symbols<CR>", "Document Symbols" },
					S = { "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", "Workspace Symbols" },
					w = { "<cmd>Telescope lsp_document_diagnostics<CR>", "Document Diagnostics" },
					W = { "<cmd>Telescope lsp_workspace_diagnostics<CR>", "Workspace Diagnostics" },
					["/"] = { '<Plug>(comment_toggle_linewise)', "Comment" },
				},
			}, { buffer = ev.buf })
		end
	})
else
	print "Failed to load lspconfig..."
end

local lsp_lines_p, lsp_lines = pcall(require, "lsp_lines")
if lsp_lines_p then
	lsp_lines.setup()
	vim.diagnostic.config({
		-- disable virtual text
		virtual_text = false,
		-- show signs
		signs = {
			active = {
				{ name = "DiagnosticSignError", text = "" },
				{ name = "DiagnosticSignWarn", text = "" },
				{ name = "DiagnosticSignHint", text = "" },
				{ name = "DiagnosticSignInfo", text = "" },
			}
		},
		update_in_insert = false,
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
	})
else
	print "Failed to load lsp_lines..."
end

local trouble_p, trouble = pcall(require, "trouble")
if trouble_p then
	whichkey.register({
		["<leader>"] = {
			["<localleader>"] = {
				t = {
					name = "Trouble",
					d = { function() trouble.toggle("document_diagnostics") end, "Document Diagnostics" },
					w = { function() trouble.toggle("workspace_diagnostics") end, "Workspace Diagnostics" },
					q = { function() trouble.toggle("quickfix") end, "Quickfix List" },
					l = { function() trouble.toggle("loclist") end, "Location List" },
					t = { trouble.toggle, "Toggle Trouble" },
				}
			}
		}
	})
else
	print "Failed to load trouble..."
end

local none_p, none = pcall(require, "null-ls")
if none_p then
	local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
	local hover = none.builtins.hover
	local formatting = none.builtins.formatting
	local diagnostics = none.builtins.diagnostics

	none.setup({
		on_attach = function(client, bufnr)
			if client.supports_method("textDocument/formatting") then
				vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
				vim.api.nvim_create_autocmd("BufWritePre", {
					group = augroup,
					buffer = bufnr,
					callback = function()
						-- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
						-- on later neovim version, you should use vim.lsp.buf.format({ async = false }) instead
						vim.lsp.buf.format({
							filter = function(client)
								return client.name == "null-ls"
							end,
							bufnr = bufnr,
							async = false
						})
					end,
				})
			end
		end,
		sources = {
			-- General
			formatting.trim_whitespace,
			formatting.trim_newlines,

			-- C/C++
			diagnostics.clang_check,
			formatting.clang_format.with({
				extra_args = { "-i", '-style={"IndentWith:4, BreakBeforeBraces: Allman"}' }
			}),

			-- Nix
			diagnostics.statix,
			diagnostics.deadnix,
			formatting.alejandra,

			-- Haskell
			formatting.stylish_haskell,
			formatting.cabal_fmt,

			-- Lua
			diagnostics.luacheck,

			-- Fish
			diagnostics.fish,
			formatting.fish_indent,

			-- Java
			diagnostics.checkstyle.with({
				extra_args = { "-c", "/google_checks.xml" },
			}),

			-- Make
			diagnostics.checkmake,

			-- Python
			diagnostics.mypy,
			formatting.black,
			formatting.isort,

			-- LaTeX
			diagnostics.chktex,
			diagnostics.proselint,
			formatting.latexindent,
			hover.dictionary,

			-- Rust
			formatting.rustfmt,

			-- Bash
			formatting.shfmt
		}
	})
else
	print "Failed to load none-ls..."
end

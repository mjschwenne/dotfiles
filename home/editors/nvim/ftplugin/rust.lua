local bufnr = vim.api.nvim_get_current_buf()

local whichkey = require("which-key")

whichkey.register({
	['<leader>'] = {
		['<localleader>'] = {
			m = {
				name = "Rust",
				c = { vim.cmd.RustLsp('crateGraph'), "Crate Graph" },
				d = { vim.cmd.RustLsp { 'moveItem', 'down' }, 'Move Item Down' },
				e = { vim.cmd.RustLsp('explainError'), 'Explain Error'},
				D = { vim.cmd.RustLsp('debuggables'), 'Debuggable' },
				h = { vim.cmd.RustLsp { 'hover', 'actions' }, "Hover" },
				H = { vim.cmd.RustLsp { 'hover', 'range' }, "Hover Range" },
				j = { vim.cmd.RustLsp('joinLines'), "Join Lines" },
				m = { vim.cmd.RustLsp('expandMacro'), "Expand Macro" },
				o = { vim.cmd.RustLsp('openCargo'), "Open cargo.toml" },
				p = { vim.cmd.RustLsp('parentModule'), "Open Parent Module" },
				r = { vim.cmd.RustLsp('runnables'), "Run" },
				s = { vim.cmd.RustLsp('ssr'), "Structural Search Replace" },
				u = { vim.cmd.RustLsp { 'moveItem', 'up' }, 'Move Item Up' },
			}
		}
	}
}, {buffer = bufnr})

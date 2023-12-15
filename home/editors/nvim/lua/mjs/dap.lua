local dap_p, dap = pcall(require, "dap")
if dap_p then
	dap.adapters.lldb = {
		type = 'executable',
		command = '/etc/profiles/per-user/mjs/bin/lldb-vscode',
		name = 'lldb',
	}
	dap.configurations.cpp = {
		{
			name = 'Launch',
			type = 'lldb',
			request = 'launch',
			program = function()
				return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
			end,
			cwd = '${workspaceFolder}',
			stopOnEntry = false,
			args = {},
		}
	}
	dap.configurations.c = dap.configurations.cpp
	dap.configurations.rust = {
		{
			name = 'Launch',
			type = 'lldb',
			request = 'launch',
			program = function()
				return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
			end,
			cwd = '${workspaceFolder}',
			stopOnEntry = false,
			args = {},
			initCommands = function()
				-- Find out where to look for the pretty printer Python module
				local rustc_sysroot = vim.fn.trim(vim.fn.system('rustc --print sysroot'))

				local script_import = 'command script import "' .. rustc_sysroot .. '/lib/rustlib/etc/lldb_lookup.py"'
				local commands_file = rustc_sysroot .. '/lib/rustlib/etc/lldb_commands'

				local commands = {}
				local file = io.open(commands_file, 'r')
				if file then
					for line in file:lines() do
						table.insert(commands, line)
					end
					file:close()
				end
				table.insert(commands, 1, script_import)

				return commands
			end,
		}
	}

	require("which-key").register({
		['<leader>'] = {
			d = {
				name = 'Debug',
				b = { dap.toggle_breakpoint, "Toggle Breakpoint" },
				c = { dap.continue, "Continue Debugging" },
				i = { dap.step_into, "Step Into" },
				o = { dap.step_over, "Step Over" },
				O = { dap.step_out, "Step Out" },
				r = { dap.run_last, "Run Last Configuration" },
				R = { dap.repl.open, "Open REPL" },
			}
		}
	})
else
	print "Failed to load dap..."
end

local dap_vt_p, dap_vt = pcall(require, "nvim-dap-virtual-text")
if dap_vt_p then
	dap_vt.setup({
		virt_text_win_col = 60,
		commented = true,
	})
end

local telescope_p, telescope = pcall(require, "telescope")
if telescope_p then
	telescope.load_extension('dap')
end

local dap_ui_p, dap_ui = pcall(require, "dapui")
if dap_ui_p then
	dap_ui.setup()
	dap.listeners.after.event_initialized["dapui_config"] = function()
		dap_ui.open()
	end
	dap.listeners.before.event_terminated["dapui_config"] = function()
		dap_ui.close()
	end
	dap.listeners.before.event_exited["dapui_config"] = function()
		dap_ui.close()
	end
else
	print "Failed to load dap ui..."
end

local dap_py_p, dap_py = pcall(require, "dap-python")
if dap_py_p then
	dap_py.setup('/run/current-system/sw/bin/python')
else
	print "Failed to load dap-python..."
end

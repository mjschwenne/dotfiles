local dap_p, dap = pcall(require, "dap")
if dap_p then
else
	print "Failed to load dap..."
end

local dap_vt_p, dap_vt = pcall(require, "nvim-dap-virtual-text")
if dap_vt_p then
	dap_vt.setup({
		virt_text_win_col = 80,
		commented = true,
	})
end

local telescope_p, telescope = pcall(require, "telescope")
if telescope_p then
	telescope.load_extension('dap')
	-- TODO setup keybindings
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
end

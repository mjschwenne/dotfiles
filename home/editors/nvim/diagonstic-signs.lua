-- Set Diagnostic symbols
local signs = {
	{ name = "DiagnosticSignError", text = "" },
	{ name = "DiagnosticSignWarn", text = "" },
	{ name = "DiagnosticSignHint", text = "" },
	{ name = "DiagnosticSignInfo", text = "" },
	{ name = "DapBreakpoint", text = "" },
	{ name = "DapBreakpointCondition", text = "" },
	{ name = "DapLopPoint", text = "" },
	{ name = "DapStopped", text = "" },
	{ name = "DapBreakpointRejected", text = "" },
}

vim.diagnostic.config({
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = "󰅚 ",
			[vim.diagnostic.severity.WARN] = "󰀪 ",
		},
	},
	float = {
		border = "rounded",
	},
})

vim.api.nvim_create_autocmd({ "CursorHold" }, {
	pattern = "*",
	callback = function()
		for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
			if vim.api.nvim_win_get_config(winid).zindex then
				return
			end
		end
		vim.diagnostic.open_float({
			scope = "cursor",
			focusable = false,
			close_events = {
				"CursorMoved",
				"CursorMovedI",
				"BufHidden",
				"InsertCharPre",
				"WinLeave",
			},
		})
	end,
})

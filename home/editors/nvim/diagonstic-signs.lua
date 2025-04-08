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

for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

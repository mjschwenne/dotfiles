---@type LazySpec
return {
  "rebelot/heirline.nvim",
  opts = function(_, opts)
    local status = require "astroui.status"
    opts.statusline = { -- statusline
      hl = { fg = "fg", bg = "bg" },
      status.component.mode {
        mode_text = { padding = { left = 1, right = 1 }, hl = { fg = "#1e1e2e" } },
      }, -- add the mode text
      status.component.diagnostics(),
      status.component.file_info(), -- { filename = true },
      status.component.git_branch(),
      status.component.git_diff(),
      status.component.fill(),
      status.component.cmd_info(),
      status.component.fill(),
      status.component.lsp(),
      status.component.virtual_env(),
      status.component.treesitter(),
      status.component.nav(),
    }
  end,
}

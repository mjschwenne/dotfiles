-- Customize None-ls sources

---@type LazySpec
return {
  "nvimtools/none-ls.nvim",
  opts = function(_, config)
    -- config variable is the default configuration table for the setup function call
    -- local null_ls = require "null-ls"
    local null_ls = require "null-ls"
    local code_actions = null_ls.builtins.code_actions
    local diagnostics = null_ls.builtins.diagnostics
    local formatting = null_ls.builtins.formatting
    local hover = null_ls.builtins.hover

    -- Check supported formatters and linters
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/formatting
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
    config.sources = {
      -- General 
      code_actions.gitsigns,
      code_actions.refactoring,

      -- Bash 
      formatting.shfmt,

      -- Fish
      diagnostics.fish,
      formatting.fish_indent,

      -- Java 
      formatting.google_java_format,

      -- JavaScript
      formatting.prettier,

      -- LaTeX 
      diagnostics.proselint,
      hover.dictionary,

      -- Lua
      formatting.stylua,

      -- Make 
      diagnostics.checkmake,

      -- Nix
      code_actions.statix,
      formatting.alejandra,

      -- Python
      diagnostics.mypy,
      formatting.black,
      formatting.isort,
    }
    return config -- return final config table
  end,
}

return {
  "nvim-orgmode/orgmode",
  dependencies = {
    { "nvim-treesitter/nvim-treesitter", lazy = true },
  },
  event = "VeryLazy",
  config = function()
    -- Setup orgmode
    require("orgmode").setup {
      org_agenda_files = "~/Documents/agenda/*.org",
      org_default_notes_file = "~/Documents/agenda/inbox.org",
    }
  end,
}

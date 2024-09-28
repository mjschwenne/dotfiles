---@type LazySpec
return {
  "whonore/Coqtail",
  enable = true,
  ft = "coq",
  keys = {
    { "<localleader>mc", "<cmd>CoqStart<cr>", ft = "coq", desc = "Start Coqtail" },
    { "<localleader>me", "<cmd>CoqJumpToError<cr>", ft = "coq", desc = "Jump to Error" },
    { "<localleader>mi", "<cmd>CoqInterrupt<cr>", ft = "coq", desc = "Interrupt Coq" },
    { "<localleader>mn", "<cmd>CoqNext<cr>", ft = "coq", desc = "Input next sentence" },
    { "<localleader>mp", "<cmd>CoqUndo<cr>", ft = "coq", desc = "Undo last sentence" },
    { "<localleader>ms", "<cmd>CoqStop<cr>", ft = "coq", desc = "Stop Coqtail" },
    { "<localleader>mt", "<cmd>CoqToTop<cr>", ft = "coq", desc = "Rewind File" },
  },
  -- main = function(opts)
  --   vim.cmd "highlight CoqtailChecked guibg=#60845d"
  --   vim.cmd "highlight CoqtailSent guibg=#a6e3a1"
  --   vim.cmd "highlight CoqtailError guibg=#844b5b"
  -- end,
}

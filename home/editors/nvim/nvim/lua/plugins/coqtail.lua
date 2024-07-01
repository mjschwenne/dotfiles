---@type LazySpec
return {
  "whonore/Coqtail",
  enable = true,
  ft = "coq",
  keys = {
    { "<localleader>c", "<cmd>CoqStart<cr>", ft = "coq", desc = "Start Coqtail" },
    { "<localleader>e", "<cmd>CoqJumpToError<cr>", ft = "coq", desc = "Jump to Error" },
    { "<localleader>i", "<cmd>CoqInterrupt<cr>", ft = "coq", desc = "Interrupt Coq" },
    { "<localleader>n", "<cmd>CoqNext<cr>", ft = "coq", desc = "Input next sentence" },
    { "<localleader>p", "<cmd>CoqUndo<cr>", ft = "coq", desc = "Undo last sentence" },
    { "<localleader>s", "<cmd>CoqStop<cr>", ft = "coq", desc = "Stop Coqtail" },
    { "<localleader>t", "<cmd>CoqToTop<cr>", ft = "coq", desc = "Rewind File" },
  },
  -- main = function(opts)
  --   vim.cmd "highlight CoqtailChecked guibg=#60845d"
  --   vim.cmd "highlight CoqtailSent guibg=#a6e3a1"
  --   vim.cmd "highlight CoqtailError guibg=#844b5b"
  -- end,
}

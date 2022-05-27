local opts = { noremap = true, silent = true } -- Default keymap options
local term_opts = { silent = true } -- Default keymap options for terminal keymaps

local map = vim.api.nvim_set_keymap -- Short the function name

-- Remap the leader key to be the space bar
map("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
-- 	normal mode       -> "n"
-- 	insert mode       -> "i"
-- 	visual mode       -> "v"
-- 	visual block mode -> "x"
-- 	terminal mode     -> "t"
-- 	command mode      -> "c"

-- Normal Mode --
-- Remap split navigation
map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

-- File Explorer
map("n", "<leader>e", ":NvimTreeToggle<cr>", opts)

-- Resize splits with arrow keys
map("n", "<C-Up>", ":resize +2<cr>", opts)
map("n", "<C-Down>", ":resize -2<cr>", opts)
map("n", "<C-Left>", ":vertical resize -2<cr>", opts)
map("n", "<C-Right>", ":vertical resize +2<cr>", opts)

-- Telescope
map("n", "<leader>o", "<cmd>lua require'telescope.builtin'.find_files(require('telescope.themes').get_dropdown({ previewer = false }))<cr>", opts)
map("n", "<leader>s", "<cmd>Telescope live_grep<cr>", opts)

-- LSP related
map("n", "<leader>f", "<cmd>Format<cr>", opts)
map("n", "<leader>r", "<cmd>vim.lsp.buf.rename()<cr>", opts)

-- Visual Mode --
-- Stay in indent mode
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- Put in visual mode doesn't override the clipboard
map("v", "p", '"_dP', opts)

-- Visual Block Mode --
-- Move text up and down --
map("x", "J", ":move '>+1<cr>gv-gv", opts)
map("x", "K", ":move '<-2<cr>gv-gv", opts)
map("x", "<A-j>", ":move '>+1<cr>gv-gv", opts)
map("x", "<A-k>", ":move '<-2<cr>gv-gv", opts)

-- Terminal Mode --
-- Align with split navigation
map("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
map("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
map("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
map("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- Highlight on Yank
vim.cmd([[
	augroup highlight_yank
	autocmd!
	au TextYankPost * silent! lua vim.highlight.on_yank({higroup="Visual", timeout=200})
	augroup END
]])

local status_ok, catppuccin = pcall(require, "catppuccin")

-- Ensure that the colorscheme is avaiable
if not status_ok then
	print "Failed to load colorscheme..."
	return
end

vim.g.catppuccin_flavour = "mocha"
catppuccin.setup()

vim.cmd.colorscheme "catppuccin"

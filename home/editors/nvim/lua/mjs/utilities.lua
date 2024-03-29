local colorizer_p, colorizer = pcall(require, "colorizer")
if colorizer_p then
	colorizer.setup()
	require("which-key").register({
		["<leader>"] = {
			t = {
				name = "Toggle",
				c = { "<cmd>ColorizerToggle<CR>", "Colorizer" }
			}
		}
	})
else
	print "Failed to load nvim-colorizer..."
end

local comment_p, comment = pcall(require, "Comment")
if comment_p then
	comment.setup()
else
	print "Failed to load comment.nvim..."
end

local trim_p, trim = pcall(require, "trim")
if trim_p then
	trim.setup()
else
	print "Failed to load trim.nvim"
end

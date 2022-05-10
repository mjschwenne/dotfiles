-- :help options for more information about each option.
local options = {
	backup = false,								-- Do not create a ~ backup file
	clipboard = "unnamedplus",					-- Use the '+' regiester for clipboard
	cmdheight = 1,								-- Height of the command window at the bottom
	completeopt = { "menuone", "noselect" },	-- When to use a popup menu
	conceallevel = 0,							-- So that `` is visible in markdown files
	fileencoding = "utf-8",						-- Write all files in utf-8
	hlsearch = true,							-- Highlight all matches during a search
	ignorecase = true,							-- Ignore case while searaching
	pumheight = 10,								-- Height of the popup menu
	smartcase = true,							-- Override ignorecase if search has captial letter
	smartindent = true,							-- Smart new line indentation
	autoindent = true,							-- Recommended with smartindent
	splitbelow = true,							-- Horitzontal splits always appear below
	splitright = true,							-- Vertical splits always appear right
	timeoutlen = 500,							-- Timeout for command key sequences
	tabstop = 4,								-- Tab width
	shiftwidth = 4,								-- Tab width if replaced by spaces
	history = 50,								-- Number of previous commands and searchs remembered
	autochdir = true,							-- Keep the cwd to the directory of the active buffer
	guicursor = "i:block",						-- Use block cursor in insert mode
	number = true,								-- Line numbers
	relativenumber = false,						-- Relative line numbers
	numberwidth = 4,							-- Number of characters for the line numbers
	signcolumn = "number",						-- How to display the sign column
	scrolloff = 8,								-- Number of lines above/below scrolling cursor
	sidescrolloff = 8,							-- Number of character left/right scrolling cursor
	wrap = false,								-- Do not wrap lines
	showmode = false,							-- Do not show mode in command gutter
}

-- Appending some options
vim.opt.shortmess:append "c"					-- Suppress complation messages
vim.opt.whichwrap:append "<,>,[,]"				-- Allow <left> and <right> to move lines
vim.opt.iskeyword:append "-"					-- Consider '-' as part of a word

-- Apply the options
for k, v in pairs(options) do
	vim.opt[k] = v
end

local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system {
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	}
	print "Installing packer close and reopen Neovim..."
	vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
	augroup packer_user_config
		autocmd!
		autocmd BufWritePost plugins.lua source <afile> | PackerSync
	augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	print "Failed to load packer..."
	return
end

-- Have packer use a popup window
packer.init {
	display = {
		open_fn = function()
			return require("packer.util").float { border = "rounded" }
		end,
	},
}

-- Install the plugins
return packer.startup(function(use)
	-- List of plugins
	use "wbthomason/packer.nvim" 		-- Have packer manage itself
	use "nvim-lua/popup.nvim" 			-- An implementation of the Popup API from vim in Neovim
	use "nvim-lua/plenary.nvim" 		-- Useful lua functions used by lots of plugins

	-- Syntax highlighting
	use "fladson/vim-kitty"				-- Kitty conf syntax highlighting
	use "lervag/vimtex" 				-- LaTex
	use "luizribeiro/vim-cooklang"      -- cooklang

	-- cmp plugins
	use "hrsh7th/nvim-cmp" 				-- Completion plugin
	use "hrsh7th/cmp-buffer" 			-- Buffer completions
	use "hrsh7th/cmp-path" 				-- Path completions
	use "hrsh7th/cmp-cmdline"		 	-- Cmdline completions
	use "saadparwaiz1/cmp_luasnip" 		-- Snippet completions
	use "hrsh7th/cmp-nvim-lsp"			-- LSP completions
	use "hrsh7th/cmp-nvim-lua"			-- nvim lua completions

	-- Keybinding help
	use "folke/which-key.nvim" 			-- Keybinding help menu

	-- snippets
	use "L3MON4D3/LuaSnip" 				-- Snippet engine
	use "rafamadriz/friendly-snippets" 	-- Snippet collection

	-- LSP
	use "neovim/nvim-lspconfig" 			-- Enable LSP
	use "williamboman/nvim-lsp-installer" 	-- Simple language server installer
	use "jose-elias-alvarez/null-ls.nvim" 	-- for formatters and linters

	-- Telescope
	use "nvim-telescope/telescope.nvim"				-- Fuzzy finder
	use "nvim-telescope/telescope-media-files.nvim"	-- View media files in fuzzy finder

	-- Treesitter
	use {									-- Better syntax highlighting
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	}

	-- Autopair
	use "windwp/nvim-autopairs" 			-- Autopairs, integrates with both cmp and treesitter

	-- Comments
	use "numToStr/Comment.nvim"							-- Comment plugin
	use 'JoosepAlviste/nvim-ts-context-commentstring' 	-- Context aware commenting (usful in embedded languages)

	-- File explorer
	use 'kyazdani42/nvim-web-devicons'		-- Support for icons in file tree
	use 'kyazdani42/nvim-tree.lua'			-- File tree plugin itself

	-- Performance
	use 'lewis6991/impatient.nvim'			-- Performance booster

	-- Cosmetic changes
	use "nvim-lualine/lualine.nvim"					-- This is only one of many status line plugins
	use "goolord/alpha-nvim" 						-- Splash screen
	use { "catppuccin/nvim", as = "catppuccin" }	-- Catppuccin colorscheme

	-- Automatically set up the configuration after cloning packer.nvim
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)

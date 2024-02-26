local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
	augroup packer_user_config
		autocmd!
		autocmd BufWritePost plugins.lua source <afile> | PackerSync
	augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	print("Failed to load packer...")
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

-- Install the plugins
return packer.startup(function(use)
	-- List of plugins
	use("wbthomason/packer.nvim") -- Have packer manage itself
	use("nvim-lua/popup.nvim") -- An implementation of the Popup API from vim in Neovim
	use("nvim-lua/plenary.nvim") -- Useful lua functions used by lots of plugins

	-- Cosmetic changes
	use("nvim-lualine/lualine.nvim") -- This is only one of many status line plugins
	use("arkav/lauline-lsp-progress")
	use("goolord/alpha-nvim") -- Splash screen
	use({ "catppuccin/nvim", as = "catppuccin" }) -- Catppuccin colorscheme
	use("norcalli/nvim-colorizer.lua")

	-- Syntax
	use("mfussenegger/nvim-jdtls")

	-- cmp plugins
	use("ms-jpq/coq_nvim")
	use("ms-jpq/coq.thirdparty")
	use("ms-jpq/coq.artifacts")

	-- Keybinding help
	use("folke/which-key.nvim") -- Keybinding help menu

	-- LSP
	use("neovim/nvim-lspconfig") -- Enable LSP
	use("williamboman/mason.nvim") -- language server installer``
	use("RubixDev/mason-update-all") -- update all LSPs
	use("williamboman/mason-lspconfig.nvim") -- Brigde between nvim-lspconfig and mason
	use("ErichDonGubler/lsp_lines.nvim")
	use("folke/trouble.nvim")
	use("nvimtools/none-ls.nvim")

	-- Telescope
	use("nvim-telescope/telescope.nvim") -- Fuzzy finder
	use("nvim-telescope/telescope-media-files.nvim") -- View media files in fuzzy finder
	use("nvim-telescope/telescope-ui-select.nvim") -- See code actions in fuzzy finder

	-- Treesitter
	use({ -- Better syntax highlighting
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	})

	-- Autopair
	use("windwp/nvim-autopairs") -- Autopairs, integrates with both cmp and treesitter

	-- Comments
	use("numToStr/Comment.nvim") -- Comment plugin
	-- use("JoosepAlviste/nvim-ts-context-commentstring") -- Context aware commenting (usful in embedded languages)

	-- File explorer
	use("kyazdani42/nvim-web-devicons") -- Support for icons in file tree
	use("kyazdani42/nvim-tree.lua") -- File tree plugin itself

	-- Automatically set up the configuration after cloning packer.nvim
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)

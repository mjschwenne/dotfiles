vim.g.skip_ts_context_commentstring_module = true

local treesitter_p, treesitter = pcall(require, "nvim-treesitter.configs")
if treesitter_p then
	treesitter.setup {
		highlight = {
			enable = true,	 				-- false will disable the whole extension
			disable = { "" }, 				-- List of language that will be disabled
			additional_vim_regex_highlighting = true,
		},
		indent = { enable = true, disable = { "yaml" } },
		context_commentstring = {
			enable = true,
			enable_autocmd = false,
		},
		endwise = { enable = true, },
		textobjects = {
			select = {
				enable = true,
				lookahead = true,
			}
		},
		textsubjects = {
			enable = true,
			prev_selection = ',',
			keymaps = {
				['.'] = {'textsubjects-smart', desc = "Select Containers"},
				[';'] = {'textsubjects-container-outer', desc = "Select Outside Containers"},
				['i;'] = { 'textsubjects-container-inner', desc = "Select Inside Containers" },
			},
		},
		refactor = {
			enable = true,
			highlight_definitions = {
				enable = true,
				clear_on_cursor_move = true,
			},
			smart_rename = {
				enable = true,
				keymaps = {
					smart_rename = false,
				}
			}
		},
	}
	local whichkey = require("which-key")
	whichkey.register({ ["<leader>"] = {
		["<localleader>"] = {
			T = {
				name = "Treesiter",
				r = { require"nvim-treesitter-refactor.smart_rename".smart_rename, "Rename" },
				d = { require"nvim-treesitter-refactor.navigation".goto_definition, "Point Definition" },
				D = { require"nvim-treesitter-refactor.navigation".list_definitions, "All Definitions" },
				o = { require"nvim-treesitter-refactor.navigation".list_definitions_toc, "Definition TOC" },
				n = { require"nvim-treesitter-refactor.navigation".goto_next_usage, "Next Usage" },
				p = { require"nvim-treesitter-refactor.navigation".goto_porvious_usage, "Pervious Usage" },
			}
		}
	}})

	vim.api.nvim_set_keymap("v", "t", "<Nop>", { noremap = true, silent = true, desc = "Treesitter" })
	vim.api.nvim_set_keymap("o", "t", "<Nop>", { noremap = true, silent = true, desc = "Treesitter" })
	vim.api.nvim_set_keymap("n", "t", "<Nop>", { noremap = true, silent = true, desc = "Treesitter" })
	local textobjects = {
		t = {
			name = "Treesitter",
			a = {
				name = "Assignment",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@assignment.inner")<CR>', "Inner" },
				l = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@assignment.lhs")<CR>', "Left Side" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@assignment.outer")<CR>', "Outer" },
				r = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@assignment.rhs")<CR>', "Right Side" },
			},
			b = {
				name = "Block",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@block.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@block.outer")<CR>', "Outer" },
			},
			f = {
				name = "Function",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@function.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@function.outer")<CR>', "Outer" },
			},
			c = {
				name = "Class",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@class.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@class.outer")<CR>', "Outer" },
			},
			C = {
				name = "Call",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@call.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@call.outer")<CR>', "Outer" },
			},
			l = {
				name = "Loop",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@loop.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@loop.outer")<CR>', "Outer" },
			},
			n = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@number.inner")<CR>', "Number"},
			p = {
				name = "Parameter",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@parameter.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@parameter.outer")<CR>', "Outer" },
			},
			r = {
				name = "Regex",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@regex.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@regex.outer")<CR>', "Outer" },
			},
			[';'] = {
				name = "Comment",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@comment.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@comment.outer")<CR>', "Outer" },
			},
			['?'] = {
				name = "Conditional",
				i = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@conditional.inner")<CR>', "Inner" },
				o = { '<cmd>lua require"nvim-treesitter.textobjects.select".select_textobject("@conditional.outer")<CR>', "Outer" },
			},
		}
	}
	whichkey.register(textobjects, { mode = "v"})
	whichkey.register(textobjects, { mode = "o"})
	whichkey.register({
		["["] = {
			name = "Move to Pervious",
			a =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@assignment.lhs")<CR>', "Assignment LHS"},
			b =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@block.inner")<CR>', "Block"},
			c =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@class.inner")<CR>', "Class"},
			m =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@function.inner")<CR>', "Method"},
			l =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@loop.inner")<CR>', "Loop"},
			n =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@number.inner")<CR>', "Number"},
			p =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@parameter.inner")<CR>', "Parameter"},
			r =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@regex.inner")<CR>', "Regex"},
			[';'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@comment.inner")<CR>', "Comment"},
			['/'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_start("@conditional.inner")<CR>', "Conditional"} ,
			[':'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@comment.inner")<CR>', "Comment"},
			['?'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@conditional.inner")<CR>', "Conditional"} ,
			A =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@assignment.rhs")<CR>', "Assignment (End)"},
			B =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@block.inner")<CR>', "Block (End)"},
			C =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@class.inner")<CR>', "Class (End)"},
			M =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@function.inner")<CR>', "Method (End)"},
			L =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@loop.inner")<CR>', "Loop (End)"},
			N =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@number.inner")<CR>', "Number"},
			P =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@parameter.inner")<CR>', "Parameter (End)"},
			R =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_previous_end("@regex.inner")<CR>', "Regex (End)"},
		},
		["]"] = {
			name = "Move to Next",
			a =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@assignment.lhs")<CR>', "Assignment LHS"},
			b =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@block.inner")<CR>', "Block"},
			c =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@class.inner")<CR>', "Class"},
			m =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@function.inner")<CR>', "Method"},
			l =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@loop.inner")<CR>', "Loop"},
			n =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@number.inner")<CR>', "Number"},
			p =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@parameter.inner")<CR>', "Parameter"},
			r =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@regex.inner")<CR>', "Regex"},
			[';'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@comment.inner")<CR>', "Comment"},
			['/'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_start("@conditional.inner")<CR>', "Conditional"} ,
			[':'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@comment.inner")<CR>', "Comment"},
			['?'] = { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@conditional.inner")<CR>', "Conditional"} ,
			A =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@assignment.rhs")<CR>', "Assignment (End)"},
			B =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@block.inner")<CR>', "Block (End)"},
			C =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@class.inner")<CR>', "Class (End)"},
			M =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@function.inner")<CR>', "Method (End)"},
			L =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@loop.inner")<CR>', "Loop (End)"},
			N =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@number.inner")<CR>', "Number"},
			P =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@parameter.inner")<CR>', "Parameter (End)"},
			R =     { '<cmd>lua require"nvim-treesitter.textobjects.move".goto_next_end("@regex.inner")<CR>', "Regex (End)"},
		}
	})
else
	print "Failed to load treesitter..."
end

-- local ts_context_p, ts_context = pcall(require, "treesitter-context")
-- if ts_context_p then
-- 	ts_context.setup()
-- else
-- 	print "Failed to load treesitter-context..."
-- end

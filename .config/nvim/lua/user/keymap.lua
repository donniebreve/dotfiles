-- Reference keymaps: https://github.com/LunarVim/Neovim-from-scratch/blob/02-keymaps/lua/user/keymaps.lua

local set = vim.keymap.set
local options = { noremap = true }

-- Leader key
set("", "<space>", "<nop>", options)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- General
----
set({ "n" }, "<leader>qq", "<cmd>qa<cr>", options) -- quit nvim
set({ "n" }, "<leader>fs", "<cmd>w<cr>", options)  -- save file
set({ "n" }, "<Esc>", "<cmd>noh<cr><esc>", { desc = "Escape and clear hlsearch", noremap = true })

-- Tabs
-- Tabs in vim are not like workspaces exactly (even though they contain a list of buffers)
-- Something I want is the ability to switch between projects quickly while keeping vim open
-- Needs some more investigation here
set({ "n" }, "<leader><tab>d", "<cmd>tabclose<cr>", options)

-- Windows
set({ "n" }, "<leader>wd", "<C-w>c", options)
set({ "n" }, "<leader>ws", "<cmd>split<cr>", options)
set({ "n" }, "<leader>wv", "<cmd>vsplit<cr>", options)
set({ "n" }, "<S-A-h>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width", remap = true })
set({ "n" }, "<S-A-j>", "<cmd>resize -2<cr>", { desc = "Decrease window height", remap = true })
set({ "n" }, "<S-A-k>", "<cmd>resize -2<cr>", { desc = "Increase window height", remap = true })
set({ "n" }, "<S-A-l>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width", remap = true })

-- Searching
-- A lot of these are taken from DOOM Emacs / Spacemacs
set({ "n" }, "<leader>bn", "<cmd>bn<cr>", options)
set({ "n" }, "<leader>b]", "<cmd>bn<cr>", options)
set({ "n" }, "<leader>bp", "<cmd>bp<cr>", options)
set({ "n" }, "<leader>b[", "<cmd>bp<cr>", options)
set({ "n" }, "<leader>by", "<cmd>%y<cr>", options)

-- Editing
-- Remap for dealing with word wrap
set({ "n" }, "k", 'v:count == 0 ? "gk" : "k"', { noremap = true, expr = true, silent = true })
set({ "n" }, "j", 'v:count == 0 ? "gj" : "j"', { noremap = true, expr = true, silent = true })

-- Home / End
set({ "n", "v" }, "gh", "^", options)
set({ "n", "v" }, "gl", "$", options)

-- Redo
set({ "n" }, "U", "<C-r>", options)

-- Line movement
set({ "n", "v" }, "<A-j>", ":m .+1<cr>==", options)
set({ "n", "v" }, "<A-k>", ":m .-2<cr>==", options)
set({ "x" }, "<A-j>", ":move '>+1<cr>gv-gv", options)
set({ "x" }, "<A-k>", ":move '<-2<cr>gv-gv", options)

-- Visual shifting (does not exit Visual mode)
set({ "v" }, "<", "<gv", options)
set({ "v" }, ">", ">gv", options)

-- Make Y act like V,D
set({ "n" }, "Y", "y$", options)

-- Change/Delete word
--set({ "n" }, "cw", "ciw", options)
--set({ "n" }, "dw", "diw", options)

-- Rename
set({ "n" }, "gr", vim.lsp.buf.rename, options)

-- Diagnostics
set({ "n" }, "<leader>ce", "<cmd>lua vim.diagnostic.open_float()<cr>", { desc = "Error", noremap = true, silent = true })
set({ "n" }, "<leader>cE", "<cmd>Telescope diagnostics<cr>", { desc = "Error(s)", noremap = true, silent = true })
set({ "n" }, "<leader>e[", "<cmd>lua vim.diagnostic.goto_prev()<cr>", { noremap = true, silent = true })
set({ "n" }, "<leader>e]", "<cmd>lua vim.diagnostic.goto_next()<cr>", { noremap = true, silent = true })

-- Terminal
local termoptions = { noremap = true, silent = true }
set("t", "<Esc>", "<C-\\><C-N>", termoptions)

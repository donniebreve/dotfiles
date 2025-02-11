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
set({ "n" }, "<leader>wh", "<C-w>h", options)
set({ "n" }, "<leader>wj", "<C-w>j", options)
set({ "n" }, "<leader>wk", "<C-w>k", options)
set({ "n" }, "<leader>wl", "<C-w>l", options)
set({ "n" }, "<S-A-h>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width", remap = true })
set({ "n" }, "<S-A-j>", "<cmd>resize -2<cr>", { desc = "Decrease window height", remap = true })
set({ "n" }, "<S-A-k>", "<cmd>resize -2<cr>", { desc = "Increase window height", remap = true })
set({ "n" }, "<S-A-l>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width", remap = true })

-- Searching
----
-- A lot of these are taken from DOOM Emacs / Spacemacs
set({ "n" }, "<leader>bn", "<cmd>bn<cr>", options)                                                        -- navigate to the next buffer
set({ "n" }, "<leader>b]", "<cmd>bn<cr>", options)                                                        -- navigate to the next buffer
set({ "n" }, "<leader>bp", "<cmd>bp<cr>", options)                                                        -- navigate to the previous buffer
set({ "n" }, "<leader>b[", "<cmd>bp<cr>", options)                                                        -- navigate to the previous buffer

-- set({ "n" }, "<leader>bd", "<cmd>bd<cr>", options)                                                        -- kill the buffer
--set({ "n" }, "<leader>bx", "<cmd>lua require('user.scratch').toggle_scratch()<cr>", options) -- pop up scratch buffer, haven't figured this out yet
--set({ "n" }, "<leader>bX", "<cmd>lua require('user.scratch').switch_to_scratch()<cr>", options) -- switch to scratch buffer, haven't figured this out yet
--
set({ "n" }, "<leader>by", "<cmd>%y<cr>", options)                                                                       -- yank the buffer

-- Editing
-- Remap for dealing with word wrap
set({ "n" }, "k", 'v:count == 0 ? "gk" : "k"', { noremap = true, expr = true, silent = true })
set({ "n" }, "j", 'v:count == 0 ? "gj" : "j"', { noremap = true, expr = true, silent = true })

-- Home / End
set({ "n", "v" }, "gh", "^", options)
set({ "n", "v" }, "gl", "$", options)

-- Shorten ctrl+d/u
set({ "n", "v" }, "<C-d>", "10jzz", options)
set({ "n", "v" }, "<C-u>", "10kzz", options)

-- Redo
set({ "n" }, "U", "<C-r>", options)

-- Don't clobber register on visual paste
set({ "v" }, "p", '"_dp', options)
set({ "v" }, "P", '"_dP', options)
-- Don't clobber on delete or change
set({ "n" }, "x", '"_x', options)
set({ "n" }, "c", '"_c', options)

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
--set({ "n" }, "gr", vim.lsp.buf.rename, options)
set({ "n" }, "<leader>rr", vim.lsp.buf.rename, options)

-- Terminal
local termoptions = { noremap = true, silent = true }
set("t", "<Esc>", "<C-\\><C-N>", termoptions)

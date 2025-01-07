-- Reference keymaps: https://github.com/LunarVim/Neovim-from-scratch/blob/02-keymaps/lua/user/keymaps.lua

local set = vim.keymap.set
local options = { noremap = true }

-- Leader key
set("", "<space>", "<nop>", options)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- General
----
set({ "n" }, "<leader>qq", "<cmd>qa<CR>", options) -- quit nvim
set({ "n" }, "<leader>fs", "<cmd>w<CR>", options)  -- save file
--set({ "n" }, "<leader>e", "<cmd>Explore<CR>", options) -- open project file browser
--set({ "n" }, "<leader>pe", "<cmd>Telescope find_files<CR>", options) -- open project file browser
set({ "n" }, "<leader>fd", "<cmd>Explore<CR>", options) -- open relative file browser
set({ "n" }, "<leader>gg", "<cmd>Neogit<CR>", options)
set({ "n" }, "<Esc>", "<cmd>noh<CR><esc>", { desc = "Escape and clear hlsearch", noremap = true })

-- Tabs (Workspaces)
-- Tabs in vim are not like workspaces exactly (even though they contain a list of buffers)
-- Something I want is the ability to switch between projects quickly while keeping vim open
-- Needs some more investigation here
--set({ "n" }, "<leader><tab>.", "<cmd>Telescope telescope-tabs list_tabs<CR>", options)

-- Windows
set({ "n" }, "<leader>wd", "<C-w>c", options)
set({ "n" }, "<leader>ws", "<cmd>split<CR>", options)
set({ "n" }, "<leader>wv", "<cmd>vsplit<CR>", options)
set({ "n" }, "<leader>wh", "<C-w>h", options)
set({ "n" }, "<leader>wj", "<C-w>j", options)
set({ "n" }, "<leader>wk", "<C-w>k", options)
set({ "n" }, "<leader>wl", "<C-w>l", options)
set({ "n" }, "<S-A-h>", "<cmd>vertical resize -2<CR>", { desc = "Decrease window width", remap = true })
set({ "n" }, "<S-A-j>", "<cmd>resize -2<CR>", { desc = "Decrease window height", remap = true })
set({ "n" }, "<S-A-k>", "<cmd>resize -2<CR>", { desc = "Increase window height", remap = true })
set({ "n" }, "<S-A-l>", "<cmd>vertical resize +2<CR>", { desc = "Increase window width", remap = true })

-- Searching
----
-- A lot of these are taken from DOOM Emacs / Spacemacs
set({ "n" }, "<leader>/", "<cmd>Telescope live_grep<CR>", options)                                        -- fuzzy search all files in project (cwd)
set({ "n" }, "<leader>,", "<cmd>Telescope buffers show_all_buffers=true sort_lastused=true<CR>", options) -- search buffers, default selection is always the last buffer
set({ "n" }, "<leader>bn", "<cmd>bn<CR>", options)                                                        -- navigate to the next buffer
set({ "n" }, "<leader>b]", "<cmd>bn<CR>", options)                                                        -- navigate to the next buffer
set({ "n" }, "<leader>bp", "<cmd>bp<CR>", options)                                                        -- navigate to the previous buffer
set({ "n" }, "<leader>b[", "<cmd>bp<CR>", options)                                                        -- navigate to the previous buffer
set({ "n" }, "<leader>bd", "<cmd>bd<CR>", options)                                                        -- kill the buffer
--set({ "n" }, "<leader>bx", "<cmd>lua require('user.scratch').toggle_scratch()<CR>", options) -- pop up scratch buffer, haven't figured this out yet
--set({ "n" }, "<leader>bX", "<cmd>lua require('user.scratch').switch_to_scratch()<CR>", options) -- switch to scratch buffer, haven't figured this out yet
set({ "n" }, "<leader>by", "<cmd>%y<CR>", options)                                                                       -- yank the buffer
set({ "n" }, "<leader>:", "<cmd>lua require('telescope.builtin').command_history()<CR>", options)                        -- search previous commands
set({ "n" }, "<leader>fr", "<cmd>lua require('telescope.builtin').oldfiles()<CR>", options)                              -- search recent files
set({ "n" }, "<leader>fp", "<cmd>lua require('telescope.builtin').find_files({ cwd = '~/.config/nvim/' })<CR>", options) -- search private (config) files

-- Projects
set({ "n" }, "<leader>pf", "<cmd>lua require('telescope.builtin').find_files()<CR>", options) -- search files in project (cwd)

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
set({ "n", "v" }, "<A-j>", ":m .+1<CR>==", options)
set({ "n", "v" }, "<A-k>", ":m .-2<CR>==", options)
set({ "x" }, "<A-j>", ":move '>+1<CR>gv-gv", options)
set({ "x" }, "<A-k>", ":move '<-2<CR>gv-gv", options)

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

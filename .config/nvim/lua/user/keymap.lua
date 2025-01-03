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
--set({ "n" }, "<leader>e", "<cmd>Explore<cr>", options) -- open project file browser
--set({ "n" }, "<leader>pe", "<cmd>Telescope find_files<cr>", options) -- open project file browser
set({ "n" }, "<leader>fd", "<cmd>Drex %:h<cr>", options) -- open relative file browser
set({ "n" }, "<leader>gg", "<cmd>Neogit<cr>", options)
set({ "n" }, "<esc>", "<cmd>noh<cr><esc>", { desc = "Escape and clear hlsearch", noremap = true })

-- Tabs (Workspaces)
-- Tabs in vim are not like workspaces exactly (even though they contain a list of buffers)
-- Something I want is the ability to switch between projects quickly while keeping vim open
-- Needs some more investigation here
--set({ "n" }, "<leader><tab>.", "<cmd>Telescope telescope-tabs list_tabs<cr>", options)

-- Windows
set({ "n" }, "<leader>wc", "<C-w>c", options)
set({ "n" }, "<leader>wd", "<C-w>c", options)
set({ "n" }, "<leader>ws", "<cmd>split<cr>", options)
set({ "n" }, "<leader>wv", "<cmd>vsplit<cr>", options)
set({ "n" }, "<leader>wh", "<C-w>h", options)
set({ "n" }, "<leader>wj", "<C-w>j", options)
set({ "n" }, "<leader>wk", "<C-w>k", options)
set({ "n" }, "<leader>wl", "<C-w>l", options)
set({ "n" }, "<M-h>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width", remap = true })
set({ "n" }, "<M-j>", "<cmd>resize -2<cr>", { desc = "Decrease window height", remap = true })
set({ "n" }, "<M-k>", "<cmd>resize -2<cr>", { desc = "Increase window height", remap = true })
set({ "n" }, "<M-l>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width", remap = true })

-- Searching
----
-- A lot of these are taken from DOOM Emacs / Spacemacs
set({ "n" }, "<leader>/", "<cmd>Telescope live_grep<cr>", options)                                        -- fuzzy search all files in project (cwd)
set({ "n" }, "<leader>,", "<cmd>Telescope buffers show_all_buffers=true sort_lastused=true<cr>", options) -- search buffers, default selection is always the last buffer
set({ "n" }, "<leader>bn", "<cmd>bn<cr>", options)                                                        -- navigate to the next buffer
set({ "n" }, "<leader>b]", "<cmd>bn<cr>", options)                                                        -- navigate to the next buffer
set({ "n" }, "<leader>bp", "<cmd>bp<cr>", options)                                                        -- navigate to the previous buffer
set({ "n" }, "<leader>b[", "<cmd>bp<cr>", options)                                                        -- navigate to the previous buffer
set({ "n" }, "<leader>bd", "<cmd>bd<cr>", options)                                                        -- kill the buffer
set({ "n" }, "<leader>bk", "<cmd>bd<cr>", options)                                                        -- kill the buffer
--set({ "n" }, "<leader>bx", "<cmd>lua require('user.scratch').toggle_scratch()<cr>", options) -- pop up scratch buffer, haven't figured this out yet
--set({ "n" }, "<leader>bX", "<cmd>lua require('user.scratch').switch_to_scratch()<cr>", options) -- switch to scratch buffer, haven't figured this out yet
set({ "n" }, "<leader>by", "<cmd>%y<cr>", options)                                                                       -- yank the buffer
set({ "n" }, "<leader>:", "<cmd>lua require('telescope.builtin').command_history()<cr>", options)                        -- search previous commands
set({ "n" }, "<leader>fr", "<cmd>lua require('telescope.builtin').oldfiles()<cr>", options)                              -- search recent files
set({ "n" }, "<leader>fp", "<cmd>lua require('telescope.builtin').find_files({ cwd = '~/.config/nvim/' })<cr>", options) -- search config files
set({ "n" }, "<leader>pf", "<cmd>lua require('telescope.builtin').find_files()<cr>", options)                            -- search files in the project (cwd)

-- Editing
----
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
set({ "v" }, "p", '"_dP', options)
set({ "v" }, "P", '"_dp', options)
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
set({ "n" }, "gr", vim.lsp.buf.rename, options)
set({ "n" }, "<leader>rr", vim.lsp.buf.rename, options)

--local termoptions = { noremap = true, silent = true }
--set("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
--set("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)

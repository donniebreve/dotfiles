-- Reference options: https://github.com/LunarVim/Neovim-from-scratch/blob/master/lua/user/options.lua 

-- Mouse mode: all modes
vim.opt.mouse = "a"

-- Sync clipboard between OS and Neovim
vim.opt.clipboard = "unnamedplus"

-- Files
vim.opt.filetype = "on"
vim.opt.fileencoding = "utf-8"

-- View
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = true
vim.opt.number = true
vim.opt.numberwidth = 4
vim.opt.relativenumber = false
vim.opt.scrolloff = 2
vim.opt.sidescrolloff = 2
vim.opt.showtabline = 2
vim.opt.showmode = false
vim.opt.signcolumn = "yes"
vim.opt.wrap = true

-- Editing
vim.opt.autoindent = true
vim.opt.backspace = "start,eol,indent"
-- vim.opt.cindent = true Not sure of this one yet
vim.opt.expandtab = true
vim.opt.iskeyword.append = "-"
vim.opt.shiftwidth = 2
vim.opt.smartindent = true
vim.opt.smarttab = true
vim.opt.tabstop = 2
vim.opt.undofile = true
vim.opt.undodir = "/tmp/vim/undodir"

-- Searching
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.inccommand = "split"

-- Indent soft wrapped lines
vim.opt.breakindent = true

-- Set completeopt to have a better completion experience
vim.opt.completeopt = { "menuone", "noselect" }

-- Commands
vim.opt.showcmd = true
vim.opt.cmdheight = 1

-- Shell
if vim.loop.os_uname().sysname == 'Darwin' then
    vim.o.shell = '/bin/zsh'
elseif vim.loop.os_uname().sysname == 'Linux' then
    vim.o.shell = '/bin/fish'
end

-- Plugin manager: lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
local user_augroup = vim.api.nvim_create_augroup("user", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lazy",
  desc = "Close lazy.nvim window with <Esc>",
  callback = function()
    vim.keymap.set(
      "n",
      "<Esc>",
      function() vim.api.nvim_win_close(0, false) end,
      { buffer = true, nowait = true })
  end,
  group = user_augroup
})

-- Filetypes
vim.filetype.add({
  extension = {
    -- ['cshtml'] = 'html.cshtml.razor',
    -- ['razor'] = 'html.cshtml.razor',
    ['http'] = 'http'
  },
})

-- Markdown
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "markdown",
--   desc = "Change folding to use treesitter",
--   callback = function()
--     vim.opt.foldmethod = "expr"
--     vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
--   end,
--   group = user_augroup
-- })

-- Options and keybindings here
-- These should come before the plugins according to the documentation
require("user.option")
require("user.keymap")

-- Plugins
-- plugin repositories are stored in ~/.local/share/nvim/lazy
require("lazy").setup({
  spec = {
    -- Interface
    require("user.plugins.theme"),
    require("user.plugins.tabline"),
    require("user.plugins.statusline"),
    require("user.plugins.search"),
    require("user.plugins.explorer"),
    require("user.plugins.buffer"),
    require("user.plugins.keybindings"),
    require("user.plugins.greeter"),

    -- Editing
    require("user.plugins.jump"),
    require("user.plugins.autoindent"),
    require("user.plugins.scratch"),

    -- Coding
    require("user.plugins.git"),
    require("user.plugins.completions"),
    require("user.plugins.lsp"),
    require("user.plugins.treesitter"),
    require("user.plugins.filechanges"),

    -- Specific languages
    require("user.plugins.http"),
    require("user.plugins.lua"),
    require("user.plugins.markdown"),
  },
  checker = {
    enabled = true,
    notify = false
  },
})

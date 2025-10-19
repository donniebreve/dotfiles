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

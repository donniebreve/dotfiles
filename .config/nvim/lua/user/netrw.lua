vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 0
vim.g.netrw_browse_split = 0
vim.g.netrw_altv = 1
vim.g.netrw_winsize = 25
vim.g.netrw_chgwin=1

vim.api.nvim_create_autocmd('filetype', {
  pattern = "netrw",
  desc = "Custom mappings for netrw",
  callback = function()
    local set = vim.keymap.set
    set({ "n" }, "h", "-", { remap = true, buffer = true })
    set({ "n" }, "l", "<CR>", { remap = true, buffer = true })
    set({ "n" }, "n", "%", { remap = true, buffer = true }) -- New
    set({ "n" }, "r", "R", { remap = true, buffer = true }) -- Rename
    set({ "n" }, "x", "<Del>", { remap = true, buffer = true })
    set({ "n" }, "R", "<L-l>", { remap = true, buffer = true }) -- Refresh
    set({ "n" }, "<Esc>", "<cmd>bd<CR>", { noremap = true, buffer = true }) -- Quit
  end
})

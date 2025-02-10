return {
  "goolord/alpha-nvim",
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  event = "VimEnter",
  config = function()
    local theme = require("alpha.themes.dashboard")
    theme.section.buttons.val = {
      theme.button("e", "  New file", ":ene <BAR> startinsert <CR>"),
      theme.button("f", "󰮗  Find files", "<cmd>lua require('telescope.builtin').find_files()<cr>"),
      theme.button("r", "  Recent", "<cmd>lua require('telescope.builtin').oldfiles()<cr>"),
      theme.button("c", "  Config", "<cmd>lua require('telescope.builtin').find_files({ cwd = '~/.config/nvim/' })<cr>"),
      theme.button("p", "  Plugins", "<cmd>Lazy<cr>"),
      theme.button("q", "󰈆  Quit", "<cmd>qa<cr>"),
    }
    require("alpha").setup(theme.opts)
    -- Add startup time to the dashboard
    -- This was extremely hard to find
    vim.api.nvim_create_autocmd("user", {
      callback = function()
        local stats = require("lazy").stats()
        local ms = math.floor(stats.startuptime * 100) / 100
        theme.section.footer.val = "󱐌 Neovim loaded "
            .. stats.loaded
            .. "/"
            .. stats.count
            .. " plugins in "
            .. ms
            .. "ms"
        pcall(vim.cmd.AlphaRedraw)
      end,
    })
  end
}

return {
  "alvarosevilla95/luatab.nvim",
  lazy = true,
  event = "VimEnter",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    require("luatab").setup({
      windowCount = function() return "" end,
      separator = function() return "" end
    })
  end
}

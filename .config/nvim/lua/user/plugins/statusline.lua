-- Status line
return 
{
  "nvim-lualine/lualine.nvim",
  lazy = false,
  priority = 999,
  config = function()
    require("lualine").setup()
  end
}

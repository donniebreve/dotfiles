-- Theme
-- Previous: "mofiqul/dracula.nvim",
return {
  {
    "sho-87/kanagawa-paper.nvim",
    lazy = false,
    config = function()
      vim.cmd([[colorscheme kanagawa-paper]])
    end
  }
}

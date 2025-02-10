-- return {
--   "plasticboy/vim-markdown",
--   branch = "master",
--   dependencies = { "godlygeek/tabular" },
-- }

-- return {
--   "tadmccorkle/markdown.nvim",
--   ft = "markdown", -- or "event = "VeryLazy""
--   opts = {
--     -- configuration here or empty for defaults
--   },
-- }

return {
  "MeanderingProgrammer/render-markdown.nvim",
  lazy = true,
  ft = { "markdown" },
  dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
  opts = {}
}

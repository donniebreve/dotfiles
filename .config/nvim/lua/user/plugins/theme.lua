-- Theme
-- Previous: "mofiqul/dracula.nvim",

return {
  "sho-87/kanagawa-paper.nvim",
  lazy = false,
  config = function()
    local colors = require("kanagawa-paper.colors").setup({})
    require("kanagawa-paper").setup({
      colors = {
        theme = {
          ui = {
            tabline = {
              fg_selected = colors.palette.sumiInk3,
              bg_selected = colors.palette.dragonYellow
            },
          },
        }
      },
    })
    vim.cmd([[colorscheme kanagawa-paper]])
  end
}

-- return {
--   dir = "~/Projects/kanagawa-paper.nvim",
--   dev = { true },
--   lazy = false,
--   config = function()
--     local colors = require("kanagawa-paper.colors").setup({})
--     require("kanagawa-paper").setup({
--       colors = {
--         theme = {
--           ui = {
--             tabline = {
--               fg_selected = colors.palette.sumiInk3,
--               bg_selected = colors.palette.dragonYellow
--             },
--           },
--         }
--       },
--     })
--     vim.cmd([[colorscheme kanagawa-paper]])
--   end
-- }

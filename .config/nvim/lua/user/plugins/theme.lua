-- return {
--     "zenbones-theme/zenbones.nvim",
--     -- Optionally install Lush. Allows for more configuration or extending the colorscheme
--     -- If you don't want to install lush, make sure to set g:zenbones_compat = 1
--     -- In Vim, compat mode is turned on as Lush only works in Neovim.
--     dependencies = "rktjmp/lush.nvim",
--     lazy = false,
--     priority = 1000,
--     -- you can set set configuration options here
--     config = function()
--         -- vim.g.zenbones_darken_comments = 45
--         vim.cmd.colorscheme('kanagawabones')
--     end
-- }

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
          TelescopeSelection = { link = "CursorLine" },
          TelescopePromptNormal = { fg = colors.palette.sumiInk3, bg = colors.palette.sumiInk3 },
        }
      },
    })



	-- return {
	-- 	TelescopeTitle = { fg = theme.ui.fg_dark, bg = theme.accent.accent1, bold = true },
	-- 	TelescopeBorder = { fg = theme.ui.float.fg_border, bg = theme.ui.bg },
	-- 	TelescopeSelection = { link = "CursorLine" },
	-- 	TelescopeSelectionCaret = { link = "CursorLineNr" },
	-- 	TelescopePromptNormal = { bg = theme.ui.bg_p2 },
	-- 	TelescopePromptBorder = { fg = theme.ui.bg_p2, bg = theme.ui.bg_p2 },
	-- 	TelescopeResultsClass = { link = "Structure" },
	-- 	TelescopeResultsStruct = { link = "Structure" },
	-- 	TelescopeResultsField = { link = "@field" },
	-- 	TelescopeResultsMethod = { link = "Function" },
	-- 	TelescopeResultsVariable = { link = "@variable" },
	-- 	TelescopeResultsTitle = { fg = theme.ui.fg_dark, bg = theme.accent.accent2, bold = true },
	-- 	TelescopeResultsNormal = { fg = theme.ui.fg, bg = theme.ui.bg_p1 },
	-- 	TelescopeResultsBorder = { fg = theme.ui.bg_p2, bg = theme.ui.bg_p1 },
	-- 	TelescopePreviewTitle = { fg = theme.ui.fg_dark, bg = theme.accent.accent3, bold = true },
	-- 	TelescopePreviewNormal = { bg = theme.ui.bg },
	-- 	TelescopePreviewBorder = { bg = theme.ui.bg, fg = theme.ui.bg_p2 },
	-- }



    vim.cmd([[colorscheme kanagawa-paper]])
    vim.cmd [[highlight TelescopeSelection guibg=#2a2e36 guifg=#ffffff]]
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

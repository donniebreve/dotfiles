-- Status line
return {
  "nvim-lualine/lualine.nvim",
  lazy = true,
  event = "VeryLazy",
  config = function()
    require("lualine").setup({
      sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff", "diagnostics" },
        lualine_c = { "filename" },
        lualine_x = {
          { "encoding",   padding = { right = 0, left = 1 }, separator = "" },
          { "fileformat", padding = { right = 1, left = 1 }, separator = "" },
          { "filetype",   padding = { right = 1, left = 0 }, separator = "" }
        },
        lualine_y = {
          { "progress", padding = { right = 0, left = 1 }, separator = "" },
          { "location", padding = { right = 1, left = 0 } }
        },
        lualine_z = {
          {
            require("lazy.status").updates,
            cond = require("lazy.status").has_updates,
          },
          {
            function()
              return " " .. os.date("%R")
            end,
          }
        }
      }
    })
  end
}

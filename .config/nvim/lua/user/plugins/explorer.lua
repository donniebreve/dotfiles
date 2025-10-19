return {
  "stevearc/oil.nvim",
  lazy = true,
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    default_file_explorer = true,
    columns = { "icon", { "mtime", format = "%b %d %l:%M %p" }, "size" },
    delete_to_trash = true,
    view_options = {
      show_hidden = true
    },
    use_default_keymaps = false,
    constrain_cursor = "name",
    keymaps = {
      ["h"] = { "actions.parent", mode = "n" },
      ["l"] = { "actions.select", mode = "n" },
      ["<cr>"] = { "actions.select", mode = "n" },
      ["w"] = { "actions.cd", mode = "n" },
      ["gr"] = { "actions.refresh", mode = "n" },
      ["zh"] = { "actions.toggle_hidden", mode = "n" },
      ["q"] = { "actions.close", mode = "n" },
    }
  },
  keys = {
    { "<leader>fd", "<cmd>Oil<cr>", desc = "Explorer" },
  }
}

-- return {
--   "folke/snacks.nvim",
--   priority = 1000,
--   lazy = false,
--   opts = {
--     picker = {
--       sources = {
--         explorer = {
--           layout = { preset = "ivy" },
--           follow_file = true,
--           auto_close = true,
--           actions = {
--             explorer_up_and_collapse = function(picker)
--               picker:set_cwd(vim.fs.dirname(picker:cwd()))
--               picker:find()
--               require("snacks.explorer.tree"):close_all(picker:cwd())
--             end,
--             explorer_focus_or_confirm = function(picker, item, action)
--               if item.dir then
--                 picker:set_cwd(item._path)
--                 picker:find()
--               else
--                 require("snacks.explorer.actions").actions.confirm(picker, item, action)
--               end
--             end,
--             explorer_collapse_and_close = function(picker)
--               require("snacks.explorer.tree"):close_all(picker:cwd())
--               picker:norm(function()
--                 picker:close()
--               end)
--             end
--           },
--           win = {
--             list = {
--               keys = {
--                 ["h"] = "explorer_up_and_collapse",
--                 ["l"] = "explorer_focus_or_confirm",
--                 ["t"] = "terminal",
--                 ["q"] = "explorer_collapse_and_close"
--               }
--             }
--           }
--         }
--       }
--     },
--     explorer = {
--       enabled = true,
--       replace_netrw = true,
--     },
--   },
--   keys = {
--     { "<leader>fd", "<cmd>lua require('snacks').explorer.open({ cwd = vim.fn.expand('%:p:h') })<cr>", desc = "Explorer" },
--   }
-- }

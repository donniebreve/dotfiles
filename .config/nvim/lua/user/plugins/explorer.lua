return {
  "stevearc/oil.nvim",
  lazy = true,
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    default_file_explorer = true,
    -- mtime format is not currently working with %l, see https://github.com/stevearc/oil.nvim/issues/582
    --columns = { "icon", "permissions", "size", { "mtime", format = "%b %d %l:%M %p" } },
    columns = { "icon", "permissions", "size", { "mtime", format = "%b %d %I:%M %p" } },
    delete_to_trash = true,
    view_options = {
      show_hidden = true
    },
    use_default_keymaps = false,
    constrain_cursor = "name",
    keymaps = {
      ["h"] = { "actions.parent", mode = "n" },
      ["l"] = { "actions.select", mode = "n" },
      ["gr"] = { "actions.refresh", mode = "n" },
      ["zh"] = { "actions.toggle_hidden", mode = "n" },
      ["q"] = { "actions.close", mode = "n" },
    }
  },
  keys = {
    { "<leader>fd", "<cmd>Oil<cr>", desc = "Explorer" },
  }
}

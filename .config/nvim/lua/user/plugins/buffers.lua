-- Usage: don't mangle windows when closing buffers
return {
  "folke/snacks.nvim",
  ---@type snacks.Config
  opts = {
    -- https://github.com/folke/snacks.nvim/blob/main/docs/bufdelete.md
    bufdelete = { enabled = true },
  },
  keys = {
    { "<leader>bd", function() Snacks.bufdelete() end, desc = "Delete buffer" },
  }
}

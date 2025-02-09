-- Usage: don't mangle windows when closing buffers
return {
  "bit101/bufkill",
  lazy = false,
  keys = {
    { "<leader>bd", "<cmd>KillBuffer<cr>", desc = "Kill buffer" }
  }
}

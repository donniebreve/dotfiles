return {
  "https://git.sr.ht/~swaits/scratch.nvim",
  lazy = true,
  keys = {
    { "<leader>x", "<cmd>Scratch<cr>", desc = "Scratch Buffer", mode = "n" },
  },
  cmd = {
    "Scratch",
    "ScratchSplit",
  },
  opts = {
    buffer_name = "[Scratch]"
  },
}

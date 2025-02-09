-- Kulala: creating and editing HTTP requests
return {
  'mistweaverco/kulala.nvim',
  opts = {},
  ft = { "http" },
  keys = {
    { "<F5>", "<cmd>lua require('kulala').run()<cr>", desc = "Run", ft = { "http" } }
  }
}

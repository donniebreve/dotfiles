return {
  "folke/snacks.nvim",
  ---@type snacks.Config
  opts = {
    picker = {
      -- https://github.com/folke/snacks.nvim/blob/main/docs/picker.md
      enabled = true,
      layout = { preset = "ivy" },
    },
  },
  keys = {
    -- Find
    { "<leader>.",  function() Snacks.picker.smart() end, desc = "Smart Find Files" },
    { "<leader>,",  function() Snacks.picker.buffers({ current = false, sort_lastused = true }) end, desc = "Buffers" },
    { "<leader>:",  function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<m-x>",      function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>e",  function() Snacks.explorer() end, desc = "File Explorer" },
    { '<leader>s/', function() Snacks.picker.search_history() end, desc = "Search History" },
    -- Files
    { "<leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Find Config File" },
    { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files" },
    { "<leader>fg", function() Snacks.picker.git_files() end, desc = "Find Git Files" },
    { "<leader>fp", function() Snacks.picker.projects() end, desc = "Projects" },
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
    { "<leader>f/", function() Snacks.picker.grep() end, desc = "Grep" },
    { "<leader>f*", function() Snacks.picker.grep_word() end, desc = "Visual selection or word", mode = { "n", "x" } },
     -- Grep
    { "<leader>/",  function() Snacks.picker.lines() end, desc = "Buffer Lines" },
    { "<leader>b/", function() Snacks.picker.grep_buffers() end, desc = "Grep Open Buffers" },
    -- Help
    { "<leader>h",  function() Snacks.picker.help() end, desc = "Search help" },
  }
}

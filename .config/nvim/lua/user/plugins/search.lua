-- Telescope: provides navigation and search pop up windows
return {
  "nvim-telescope/telescope.nvim",
  lazy = false,
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local actions = require("telescope.actions")
    require("telescope").setup({
      defaults = require("telescope.themes").get_ivy({
        borderchars = {
          preview = { " ", " ", " ", " ", " ", " ", " ", " ", },
          prompt = { " ", " ", " ", " ", " ", " ", " ", " ", },
          results = { " " },
        },
        results_title = false,
        -- TODO: Investigate and open bug
        -- These are not currently working
        -- prompt_title = false,
        -- preview_title = false,
        mappings = {
          i = {
            ["<tab>"] = actions.move_selection_next,
            ["<s-tab>"] = actions.move_selection_previous,
            ["<esc>"] = actions.close,
          },
        },
      })
    })
  end,
  keys = {
    { "<leader>/",     "<cmd>Telescope live_grep<cr>",                                                      desc = "Fuzzy search CWD" },
    { "<leader>,",     "<cmd>Telescope buffers show_all_buffers=true sort_lastused=true<cr>",               desc = "Buffers" },
    { "<leader>:",     "<cmd>lua require('telescope.builtin').commands()<cr>",                              desc = "Command history" },
    { "<leader><c-r>", "<cmd>lua require('telescope.builtin').command_history()<cr>",                       desc = "Command history" },
    { "<leader>fr",    "<cmd>lua require('telescope.builtin').oldfiles()<cr>",                              desc = "Recent files" },
    { "<leader>fp",    "<cmd>lua require('telescope.builtin').find_files({ cwd = '~/.config/nvim/' })<cr>", desc = "Private (config) files" },
    { "<leader>pf",    "<cmd>lua require('telescope.builtin').find_files()<cr>",                            desc = "Find files in CWD" },
  }
}

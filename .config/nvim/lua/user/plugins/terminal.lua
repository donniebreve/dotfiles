return {
  {
    "christoomey/vim-tmux-navigator",
    lazy = false,
    cond = vim.env.TMUX ~= nil and vim.env.TMUX ~= "",
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
      "TmuxNavigatorProcessList",
    },
    keys = {
      { "<C-h>", "<cmd>TmuxNavigateLeft<cr>" },
      { "<C-j>", "<cmd>TmuxNavigateDown<cr>" },
      { "<C-k>", "<cmd>TmuxNavigateUp<cr>" },
      { "<C-l>", "<cmd>TmuxNavigateRight<cr>" },
    }
  },
  {
    "willothy/wezterm.nvim",
    lazy = false,
    cond = vim.env.TERM_PROGRAM ~= nil and vim.env.TERM_PROGRAM == "WezTerm",
    -- config = true,
    keys = {
      { "<C-h>", function() require("wezterm").switch_pane.direction("Left") end },
      { "<C-j>", function() require("wezterm").switch_pane.direction("Down") end },
      { "<C-k>", function() require("wezterm").switch_pane.direction("Up") end },
      { "<C-l>", function() require("wezterm").switch_pane.direction("Right") end },
    }
  }
}

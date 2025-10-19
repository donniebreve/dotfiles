return {
  "nvim-orgmode/orgmode",
  event = "VeryLazy",
  ft = { "org" },
  config = function()
    require("orgmode").setup({
      org_agenda_files = "~/Documents/Notes/*",
      org_default_notes_file = "~/orgfiles/refile.org",
      org_todo_keywords = { "TODO(t)", "CURRENT(c)", "HOLD(h)", "|", "DONE(d)", "REMOVED(r)" }
    })
  end,
}

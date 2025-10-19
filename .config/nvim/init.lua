require("user.plugins.lazy")

-- Options and keybindings here
-- These should come before the plugins according to the documentation
require("user.option")
require("user.keymap")

-- Plugins
-- plugin repositories are stored in ~/.local/share/nvim/lazy
require("lazy").setup({
  spec = {
    -- Interface
    require("user.plugins.theme"),
    require("user.plugins.keybindings"),
    require("user.plugins.tabline"),
    require("user.plugins.statusline"),
    require("user.plugins.terminal"),
    require("user.plugins.search"),
    require("user.plugins.explorer"),
    require("user.plugins.buffers"),
    require("user.plugins.notifications"),
    require("user.plugins.greeter"),

    -- Editing
    require("user.plugins.neoscroll"),
    require("user.plugins.jump"),
    require("user.plugins.autoindent"),
    require("user.plugins.orgmode"),
    require("user.plugins.scratch"),

    -- Coding
    require("user.plugins.motions"),
    require("user.plugins.git"),
    require("user.plugins.completions"),
    require("user.plugins.lsp"),
    require("user.plugins.treesitter"),
    require("user.plugins.filechanges"),

    -- Specific languages
    -- require("user.plugins.http"),
    require("user.plugins.lua"),
    -- require("user.plugins.markdown"),
  },
  checker = {
    enabled = true,
    notify = false
  },
})

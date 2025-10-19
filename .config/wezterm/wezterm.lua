local wezterm = require("wezterm")
local config = wezterm.config_builder()
local nvim = require("nvim")
local tabbar = require("tmux_tabbar")

config.term = "xterm-256color"
config.initial_cols = 140
config.initial_rows = 40
config.font_size = 11
config.font = wezterm.font("JetBrainsMono Nerd Font")
-- Alacritty renders the font with this hint seemingly
config.freetype_load_target = "Light"
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.audible_bell = "Disabled"
config.scrollback_lines = 3500
config.color_scheme = "kanagawa-paper-ink-lightened"
-- This should be the default
config.bold_brightens_ansi_colors = false
tabbar.configure(config)

config.keys = {
  nvim.passthrough("t", "CTRL", wezterm.action.SpawnTab("CurrentPaneDomain")),
  nvim.passthrough("x", "CTRL", wezterm.action.CloseCurrentTab({ confirm = true })),
  nvim.passthrough("p", "CTRL", wezterm.action.ActivateTabRelative(-1)),
  nvim.passthrough("n", "CTRL", wezterm.action.ActivateTabRelative(1)),
  nvim.passthrough("s", "CTRL", wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" })),
  nvim.passthrough("v", "CTRL", wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" })),
  nvim.passthrough("h", "CTRL", wezterm.action.ActivatePaneDirection("Left")),
  nvim.passthrough("j", "CTRL", wezterm.action.ActivatePaneDirection("Down")),
  nvim.passthrough("k", "CTRL", wezterm.action.ActivatePaneDirection("Up")),
  nvim.passthrough("l", "CTRL", wezterm.action.ActivatePaneDirection("Right")),
  { key = "b", mods = "CTRL", action = wezterm.action.ActivateKeyTable({ name = "prefix", one_shot = true }) }
}

config.key_tables = {
  prefix = {
    { key = "p", mods = "CTRL", action = wezterm.action.ActivateTabRelative(-1) },
    { key = "n", mods = "CTRL", action = wezterm.action.ActivateTabRelative(1) },
    { key = "s", mods = "CTRL", action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "v", mods = "CTRL", action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "d", mods = "CTRL", action = wezterm.action.CloseCurrentPane({ confirm = true }) },
  }
}

return config

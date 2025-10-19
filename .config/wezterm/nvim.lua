local wezterm = require("wezterm")

local M = {}

--- Sends the key event as is if the current process is neovim
--- @return boolean _ True if the event is passed through, false otherwise
local function _passthrough(win, pane, key, mods)
  if pane:get_user_vars() ~= nil and pane:get_user_vars().IS_NVIM == "true"
    or pane:get_foreground_process_name() ~= nil and pane:get_foreground_process_name():find("n?vim") then
    win:perform_action({ SendKey = { key = key, mods = mods } }, pane)
    return true
  end
  return false
end

--- Returns a key entry for wezterm with EmitEvent as the action
--- @return table _ A WezTerm key entry
local function _entry(key, mods, event)
  return {
    key = key,
    mods = mods,
    action = wezterm.action.EmitEvent(event),
  }
end

--- Wraps the key event so it is passed through to neovim if focused
--- @param key string
--- @param mods string
--- @param action table
--- @return table _ A WezTerm key entry
function M.passthrough(key, mods, action)
  local event = "passthrough_" .. mods:gsub("%|", "") .. key
  wezterm.on(event, function(win, pane)
    if not _passthrough(win, pane, key, mods) then
      win:perform_action(action, pane)
    end
  end)
  return _entry(key, mods, event)
end

return M

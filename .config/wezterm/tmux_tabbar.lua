local wezterm = require("wezterm")

local M = {}

--- Gets the basename of a path
--- @return string _ The basename
local function basename(s)
  local result = s:gsub("(.*[/\\])(.*)", "%2")
  return result
end

--- Shortens a path by using only the first two characters of directory names
--- @return string _ The shortened path
local function shorten_path(path)
  path = path:gsub(os.getenv("HOME"), "~")
  path = path:gsub("([^/]-)(/)", function(component, slash)
    if component == "" then return slash end
    if component == "~" then return "~" .. slash end
    return component:sub(1, 2) .. slash
  end)
  return path
end

--- Gets the current working directory as a string
--- @return string _ The current working directory
local function get_cwd(pane)
  local cwd = pane:get_current_working_dir()
  if cwd then
    if type(cwd) == "userdata" then
      --- @diagnostic disable-next-line: undefined-field
      return cwd.file_path
    else
      return cwd
    end
  else
    return ""
  end
end

--- Gets the color palette from the color scheme
--- @return table _ The color palette
local function get_palette(config)
  local palette = wezterm.get_builtin_color_schemes()[config.color_scheme]
  if not palette then
    palette = wezterm.color.load_scheme(wezterm.home_dir .. "/.config/wezterm/colors/" .. config.color_scheme .. ".toml")
  end
  return palette
end

--- Gets the tab title, taking max_width and is_active into account
--- @return string _ The tab title
local function get_tab_title(tab, config, max_width)
  local index = tab.tab_index
  if config.tab_and_split_indices_are_zero_based == false then
    index = index + 1
  end
  local process = tab.active_pane.foreground_process_name
  process = process and basename(process) or "shell"
  local title = tab.tab_title ~= nil and tab.tab_title ~= "" and tab.tab_title or process
  if max_width > 0 then
    local width = config.tab_max_width - 4 -- -3 for leading space and index, -1 for indicator
    if #title > width then
      title = title:sub(1, width - 1) .. "…"
    end
  end
  local indicator = tab.is_active and "*" or "-"
  return string.format(" %d:%s%s", index, title, indicator)
end

--- Configures the tab bar to mimic tmux
--- @param config _ The configuration builder
function M.configure(config)
  config.use_fancy_tab_bar = false
  config.tab_bar_at_bottom = true
  local palette = get_palette(config)
  if not palette then
    return
  end
  local background = palette.tab_bar and palette.tab_bar.background or palette.ansi[0]
  config.colors = {
    tab_bar = {
      background = background,
      active_tab = {
        bg_color = background,
        fg_color = palette.background
      },
      inactive_tab = {
        bg_color = background,
        fg_color = palette.background
      },
      new_tab = {
        bg_color = background,
        fg_color = palette.background
      },
    }
  }
  if config.tab_max_width == nil then
    config.tab_max_width = 24
  end
  wezterm.on(
    'format-tab-title',
    function(_tab, _tabs, _panes, _config, _hover, _max_width)
      local _title = get_tab_title(_tab, _config, _max_width)
      return {
        { Text = _title }
      }
    end)
  wezterm.on("update-right-status", function(window, pane)
    local _cwd = shorten_path(get_cwd(pane))
    local _time = wezterm.strftime("%H:%M %d-%b-%y")
    local _palette = window:effective_config().resolved_palette
    window:set_right_status(
      wezterm.format({
        { Foreground = { Color = _palette.background } },
        { Text = "\"" .. _cwd .. "\"" },
        { Text = " " },
        { Text = _time },
        { Text = " " },
      }))
  end)
end

return M

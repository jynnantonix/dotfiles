local wezterm = require 'wezterm'
local act = wezterm.action

-- Show which key table is active in the status area
wezterm.on('update-right-status', function(window, pane)
  local name = window:active_key_table()
  if name then
    name = 'TABLE: ' .. name
  end
  window:set_right_status(name or '')
end)

return {
  -- color_scheme = "EverforestDark (Gogh)",
  colors = {
    foreground = '#d3c6aa',
    background = '#333c43',

    cursor_bg = '#d3c6aa',
    cursor_fg = '#333c43',
    cursor_border = '#d3c6aa',

    selection_fg = '#d3c6aa',
    selection_bg = '#5c3f4f',

    scrollbar_thumb = '#7a8478',

    split = '#555f66',

    ansi = {
      '#505a60',
      '#e67e80',
      '#a7c080',
      '#dbbc7f',
      '#7fbbb3',
      '#d699b6',
      '#83c092',
      '#d3c6aa',
    },
    brights = {
      '#606a70',
      '#f68e90',
      '#b7d090',
      '#ebcc8f',
      '#8fcbc3',
      '#e6a9c6',
      '#93d0a2',
      '#e3d6ba',
    },

    compose_cursor = '#55544a',

    copy_mode_active_highlight_fg = { Color = '#d3c6aa' },
    copy_mode_active_highlight_bg = { Color = '#5c3f4f' },
    copy_mode_inactive_highlight_fg = { Color = '#d3c6aa' },
    copy_mode_inactive_highlight_bg = { Color = '#5d6b66' },

    quick_select_label_fg = { Color = '#d3c6aa' },
    quick_select_label_bg = { Color = '#5c3f4f' },
    quick_select_match_fg = { Color = '#d3c6aa' },
    quick_select_match_bg = { Color = '#5d6b66' },

    tab_bar = {
      background = '#3a464c',
      inactive_tab_edge = '#3a464c',

      active_tab = {
        bg_color = '#333c43',
        fg_color = '#d3c6aa',
      },
      inactive_tab = {
        bg_color = '#4d5960',
        fg_color = '#d3c6aa',
      },
      inactive_tab_hover = {
        bg_color = '#d3c6aa',
        fg_color = '#4d5960',
      },
      new_tab = {
        bg_color = '#333c43',
        fg_color = '#a7c080',
      },
      new_tab_hover = {
        bg_color = '#a7c080',
        fg_color = '#333c43',
      },
    },
  },
  default_prog = { '/opt/homebrew/bin/fish', '-l' },
  disable_default_key_bindings = true,
  font = wezterm.font('Source Code Pro', { weight = 'Medium' }),
  font_size = 10.0,
  keys = {
    { key = '(',          mods = 'LEADER',     action = act.ActivateTabRelative( -1) },
    { key = ')',          mods = 'LEADER',     action = act.ActivateTabRelative(1) },
    { key = 'Enter',      mods = 'ALT',        action = act.ToggleFullScreen },
    { key = '-',          mods = 'LEADER',     action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
    { key = '|',          mods = 'LEADER',     action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
    { key = '+',          mods = 'CTRL',       action = act.IncreaseFontSize },
    { key = '+',          mods = 'CTRL|SHIFT', action = act.IncreaseFontSize },
    { key = '-',          mods = 'CTRL',       action = act.DecreaseFontSize },
    { key = '0',          mods = 'CTRL',       action = act.ResetFontSize },
    { key = '1',          mods = 'LEADER',     action = act.ActivateTab(0) },
    { key = '2',          mods = 'LEADER',     action = act.ActivateTab(1) },
    { key = '3',          mods = 'LEADER',     action = act.ActivateTab(2) },
    { key = '4',          mods = 'LEADER',     action = act.ActivateTab(3) },
    { key = '5',          mods = 'LEADER',     action = act.ActivateTab(4) },
    { key = '6',          mods = 'LEADER',     action = act.ActivateTab(5) },
    { key = '7',          mods = 'LEADER',     action = act.ActivateTab(6) },
    { key = '8',          mods = 'LEADER',     action = act.ActivateTab(7) },
    { key = '9',          mods = 'LEADER',     action = act.ActivateTab(8) },
    { key = 'C',          mods = 'LEADER',     action = act.ActivateCopyMode },
    { key = 'C',          mods = 'SUPER',      action = act.CopyTo 'Clipboard' },
    { key = 'U',          mods = 'LEADER',     action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
    { key = 'V',          mods = 'SUPER',      action = act.PasteFrom 'Clipboard' },
    { key = 'c',          mods = 'LEADER',     action = act.ActivateCopyMode },
    { key = 'c',          mods = 'SUPER',      action = act.CopyTo 'Clipboard' },
    { key = 'l',          mods = 'LEADER',     action = act.ShowDebugOverlay },
    { key = 'p',          mods = 'LEADER',     action = act.ActivateKeyTable { name = 'pane' } },
    { key = 'r',          mods = 'LEADER',     action = act.ActivateKeyTable { name = 'resize_mode', one_shot = false } },
    { key = 's',          mods = 'LEADER',     action = act.Search 'CurrentSelectionOrEmptyString' },
    { key = 't',          mods = 'LEADER',     action = act.ActivateKeyTable { name = 'tab' } },
    { key = 'u',          mods = 'LEADER',     action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
    { key = 'v',          mods = 'SUPER',      action = act.PasteFrom 'Clipboard' },
    { key = 'phys:Space', mods = 'LEADER',     action = act.QuickSelect },
    { key = 'LeftArrow',  mods = 'LEADER',     action = act.ActivatePaneDirection 'Left' },
    { key = 'RightArrow', mods = 'LEADER',     action = act.ActivatePaneDirection 'Right' },
    { key = 'UpArrow',    mods = 'LEADER',     action = act.ActivatePaneDirection 'Up' },
    { key = 'DownArrow',  mods = 'LEADER',     action = act.ActivatePaneDirection 'Down' },


    { key = 'H',          mods = 'CTRL',         action = act.HideApplication },
    { key = 'H',          mods = 'SHIFT|CTRL',   action = act.HideApplication },
    { key = 'K',          mods = 'CTRL',         action = act.ClearScrollback 'ScrollbackOnly' },
    { key = 'K',          mods = 'SHIFT|CTRL',   action = act.ClearScrollback 'ScrollbackOnly' },
    { key = 'M',          mods = 'CTRL',         action = act.Hide },
    { key = 'M',          mods = 'SHIFT|CTRL',   action = act.Hide },
    { key = 'N',          mods = 'CTRL',         action = act.SpawnWindow },
    { key = 'N',          mods = 'SHIFT|CTRL',   action = act.SpawnWindow },
    { key = 'P',          mods = 'SHIFT|CTRL',   action = act.PaneSelect { alphabet = '', mode = 'Activate' } },
    { key = 'Q',          mods = 'CTRL',         action = act.QuitApplication },
    { key = 'Q',          mods = 'SHIFT|CTRL',   action = act.QuitApplication },
    { key = 'R',          mods = 'CTRL',         action = act.ReloadConfiguration },
    { key = 'R',          mods = 'SHIFT|CTRL',   action = act.ReloadConfiguration },
    { key = 'U',          mods = 'CTRL',         action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
    { key = 'U',          mods = 'SHIFT|CTRL',   action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
    { key = 'C',          mods = 'SHIFT|CTRL',   action = act.CopyTo 'Clipboard' },
    { key = 'C',          mods = 'SUPER',        action = act.CopyTo 'Clipboard' },
    { key = 'V',          mods = 'CTRL',         action = act.PasteFrom 'Clipboard' },
    { key = 'V',          mods = 'SHIFT|CTRL',   action = act.PasteFrom 'Clipboard' },
    { key = 'W',          mods = 'CTRL',         action = act.CloseCurrentTab { confirm = true } },
    { key = 'W',          mods = 'SHIFT|CTRL',   action = act.CloseCurrentTab { confirm = true } },
    { key = 'X',          mods = 'CTRL',         action = act.ActivateCopyMode },
    { key = 'X',          mods = 'SHIFT|CTRL',   action = act.ActivateCopyMode },
    { key = 'Z',          mods = 'CTRL',         action = act.TogglePaneZoomState },
    { key = 'Z',          mods = 'SHIFT|CTRL',   action = act.TogglePaneZoomState },
    { key = '[',          mods = 'SHIFT|SUPER',  action = act.ActivateTabRelative( -1) },
    { key = ']',          mods = 'SHIFT|SUPER',  action = act.ActivateTabRelative(1) },
    { key = '^',          mods = 'CTRL',         action = act.ActivateTab(5) },
    { key = '^',          mods = 'SHIFT|CTRL',   action = act.ActivateTab(5) },
    { key = '_',          mods = 'CTRL',         action = act.DecreaseFontSize },
    { key = '_',          mods = 'SHIFT|CTRL',   action = act.DecreaseFontSize },
    { key = 'c',          mods = 'SHIFT|CTRL',   action = act.CopyTo 'Clipboard' },
    { key = 'c',          mods = 'SUPER',        action = act.CopyTo 'Clipboard' },
    { key = 'f',          mods = 'SHIFT|CTRL',   action = act.Search 'CurrentSelectionOrEmptyString' },
    { key = 'f',          mods = 'SUPER',        action = act.Search 'CurrentSelectionOrEmptyString' },
    { key = 'h',          mods = 'SHIFT|CTRL',   action = act.HideApplication },
    { key = 'h',          mods = 'SUPER',        action = act.HideApplication },
    { key = 'k',          mods = 'SHIFT|CTRL',   action = act.ClearScrollback 'ScrollbackOnly' },
    { key = 'k',          mods = 'SUPER',        action = act.ClearScrollback 'ScrollbackOnly' },
    { key = 'l',          mods = 'SHIFT|CTRL',   action = act.ShowDebugOverlay },
    { key = 'm',          mods = 'SHIFT|CTRL',   action = act.Hide },
    { key = 'm',          mods = 'SUPER',        action = act.Hide },
    { key = 'n',          mods = 'SHIFT|CTRL',   action = act.SpawnWindow },
    { key = 'n',          mods = 'SUPER',        action = act.SpawnWindow },
    { key = 'p',          mods = 'SHIFT|CTRL',   action = act.PaneSelect { alphabet = '', mode = 'Activate' } },
    { key = 'q',          mods = 'SHIFT|CTRL',   action = act.QuitApplication },
    { key = 'q',          mods = 'SUPER',        action = act.QuitApplication },
    { key = 'r',          mods = 'SHIFT|CTRL',   action = act.ReloadConfiguration },
    { key = 'r',          mods = 'SUPER',        action = act.ReloadConfiguration },
    { key = 't',          mods = 'SHIFT|CTRL',   action = act.SpawnTab 'CurrentPaneDomain' },
    { key = 't',          mods = 'SUPER',        action = act.SpawnTab 'CurrentPaneDomain' },
    { key = 'u',          mods = 'SHIFT|CTRL',   action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
    { key = 'v',          mods = 'SHIFT|CTRL',   action = act.PasteFrom 'Clipboard' },
    { key = 'v',          mods = 'SUPER',        action = act.PasteFrom 'Clipboard' },
    { key = 'w',          mods = 'SHIFT|CTRL',   action = act.CloseCurrentTab { confirm = true } },
    { key = 'w',          mods = 'SUPER',        action = act.CloseCurrentTab { confirm = true } },
    { key = 'x',          mods = 'SHIFT|CTRL',   action = act.ActivateCopyMode },
    { key = 'z',          mods = 'SHIFT|CTRL',   action = act.TogglePaneZoomState },
    { key = '{',          mods = 'SUPER',        action = act.ActivateTabRelative( -1) },
    { key = '{',          mods = 'SHIFT|SUPER',  action = act.ActivateTabRelative( -1) },
    { key = '}',          mods = 'SUPER',        action = act.ActivateTabRelative(1) },
    { key = '}',          mods = 'SHIFT|SUPER',  action = act.ActivateTabRelative(1) },
    { key = 'phys:Space', mods = 'SHIFT|CTRL',   action = act.QuickSelect },
    { key = 'PageUp',     mods = 'SHIFT',        action = act.ScrollByPage( -1) },
    { key = 'PageUp',     mods = 'CTRL',         action = act.ActivateTabRelative( -1) },
    { key = 'PageUp',     mods = 'SHIFT|CTRL',   action = act.MoveTabRelative( -1) },
    { key = 'PageDown',   mods = 'SHIFT',        action = act.ScrollByPage(1) },
    { key = 'PageDown',   mods = 'CTRL',         action = act.ActivateTabRelative(1) },
    { key = 'PageDown',   mods = 'SHIFT|CTRL',   action = act.MoveTabRelative(1) },
    { key = 'Insert',     mods = 'SHIFT',        action = act.PasteFrom 'PrimarySelection' },
    { key = 'Insert',     mods = 'CTRL',         action = act.CopyTo 'PrimarySelection' },
    { key = 'Copy',       mods = 'NONE',         action = act.CopyTo 'Clipboard' },
    { key = 'Paste',      mods = 'NONE',         action = act.PasteFrom 'Clipboard' },
  },
  key_tables = {
    copy_mode = {
      { key = 'Tab',        mods = 'NONE',  action = act.CopyMode 'MoveForwardWord' },
      { key = 'Tab',        mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'Enter',      mods = 'NONE',  action = act.CopyMode 'MoveToStartOfNextLine' },
      { key = 'Escape',     mods = 'NONE',  action = act.CopyMode 'Close' },
      { key = 'Space',      mods = 'NONE',  action = act.CopyMode { SetSelectionMode = 'Cell' } },
      { key = '$',          mods = 'NONE',  action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = '$',          mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = ',',          mods = 'NONE',  action = act.CopyMode 'JumpReverse' },
      { key = '0',          mods = 'NONE',  action = act.CopyMode 'MoveToStartOfLine' },
      { key = ';',          mods = 'NONE',  action = act.CopyMode 'JumpAgain' },
      { key = 'F',          mods = 'NONE',  action = act.CopyMode { JumpBackward = { prev_char = false } } },
      { key = 'F',          mods = 'SHIFT', action = act.CopyMode { JumpBackward = { prev_char = false } } },
      { key = 'G',          mods = 'NONE',  action = act.CopyMode 'MoveToScrollbackBottom' },
      { key = 'G',          mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },
      { key = 'H',          mods = 'NONE',  action = act.CopyMode 'MoveToViewportTop' },
      { key = 'H',          mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
      { key = 'L',          mods = 'NONE',  action = act.CopyMode 'MoveToViewportBottom' },
      { key = 'L',          mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
      { key = 'M',          mods = 'NONE',  action = act.CopyMode 'MoveToViewportMiddle' },
      { key = 'M',          mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
      { key = 'O',          mods = 'NONE',  action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
      { key = 'O',          mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
      { key = 'T',          mods = 'NONE',  action = act.CopyMode { JumpBackward = { prev_char = true } } },
      { key = 'T',          mods = 'SHIFT', action = act.CopyMode { JumpBackward = { prev_char = true } } },
      { key = 'V',          mods = 'NONE',  action = act.CopyMode { SetSelectionMode = 'Line' } },
      { key = 'V',          mods = 'SHIFT', action = act.CopyMode { SetSelectionMode = 'Line' } },
      { key = '^',          mods = 'NONE',  action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = '^',          mods = 'SHIFT', action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = 'b',          mods = 'NONE',  action = act.CopyMode 'MoveBackwardWord' },
      { key = 'b',          mods = 'ALT',   action = act.CopyMode 'MoveBackwardWord' },
      { key = 'b',          mods = 'CTRL',  action = act.CopyMode 'PageUp' },
      { key = 'c',          mods = 'CTRL',  action = act.CopyMode 'Close' },
      { key = 'f',          mods = 'NONE',  action = act.CopyMode { JumpForward = { prev_char = false } } },
      { key = 'f',          mods = 'ALT',   action = act.CopyMode 'MoveForwardWord' },
      { key = 'f',          mods = 'CTRL',  action = act.CopyMode 'PageDown' },
      { key = 'g',          mods = 'NONE',  action = act.CopyMode 'MoveToScrollbackTop' },
      { key = 'g',          mods = 'CTRL',  action = act.CopyMode 'Close' },
      { key = 'h',          mods = 'NONE',  action = act.CopyMode 'MoveLeft' },
      { key = 'j',          mods = 'NONE',  action = act.CopyMode 'MoveDown' },
      { key = 'k',          mods = 'NONE',  action = act.CopyMode 'MoveUp' },
      { key = 'l',          mods = 'NONE',  action = act.CopyMode 'MoveRight' },
      { key = 'm',          mods = 'ALT',   action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = 'o',          mods = 'NONE',  action = act.CopyMode 'MoveToSelectionOtherEnd' },
      { key = 'q',          mods = 'NONE',  action = act.CopyMode 'Close' },
      { key = 't',          mods = 'NONE',  action = act.CopyMode { JumpForward = { prev_char = true } } },
      { key = 'v',          mods = 'NONE',  action = act.CopyMode { SetSelectionMode = 'Cell' } },
      { key = 'v',          mods = 'CTRL',  action = act.CopyMode { SetSelectionMode = 'Block' } },
      { key = 'w',          mods = 'NONE',  action = act.CopyMode 'MoveForwardWord' },
      { key = 'y',          mods = 'NONE',  action = act.Multiple { { CopyTo = 'ClipboardAndPrimarySelection' }, { CopyMode = 'Close' } } },
      { key = 'PageUp',     mods = 'NONE',  action = act.CopyMode 'PageUp' },
      { key = 'PageDown',   mods = 'NONE',  action = act.CopyMode 'PageDown' },
      { key = 'LeftArrow',  mods = 'NONE',  action = act.CopyMode 'MoveLeft' },
      { key = 'LeftArrow',  mods = 'ALT',   action = act.CopyMode 'MoveBackwardWord' },
      { key = 'RightArrow', mods = 'NONE',  action = act.CopyMode 'MoveRight' },
      { key = 'RightArrow', mods = 'ALT',   action = act.CopyMode 'MoveForwardWord' },
      { key = 'UpArrow',    mods = 'NONE',  action = act.CopyMode 'MoveUp' },
      { key = 'DownArrow',  mods = 'NONE',  action = act.CopyMode 'MoveDown' },
    },

    pane = {
      { key = 'Escape',     mods = 'NONE', action = act.PopKeyTable },
      { key = '-',          mods = 'NONE', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
      { key = '|',          mods = 'NONE', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
      { key = 'p',          mods = 'NONE', action = act.PaneSelect { alphabet = '', mode = 'Activate' } },
      { key = 'd',          mods = 'NONE', action = wezterm.action.CloseCurrentPane { confirm = true } },
      { key = 'LeftArrow',  mods = 'NONE', action = act.ActivatePaneDirection 'Left' },
      { key = 'RightArrow', mods = 'NONE', action = act.ActivatePaneDirection 'Right' },
      { key = 'UpArrow',    mods = 'NONE', action = act.ActivatePaneDirection 'Up' },
      { key = 'DownArrow',  mods = 'NONE', action = act.ActivatePaneDirection 'Down' },
    },

    resize_mode = {
      { key = 'Escape',     mods = 'NONE', action = act.PopKeyTable },
      { key = '+',          mods = 'NONE', action = act.IncreaseFontSize },
      { key = '=',          mods = 'NONE', action = act.IncreaseFontSize },
      { key = '-',          mods = 'NONE', action = act.DecreaseFontSize },
      { key = '_',          mods = 'NONE', action = act.DecreaseFontSize },
      { key = '0',          mods = 'NONE', action = act.ResetFontSize },
      { key = 'h',          mods = 'NONE', action = act.AdjustPaneSize { 'Left', 1 } },
      { key = 'l',          mods = 'NONE', action = act.AdjustPaneSize { 'Right', 1 } },
      { key = 'k',          mods = 'NONE', action = act.AdjustPaneSize { 'Up', 1 } },
      { key = 'j',          mods = 'NONE', action = act.AdjustPaneSize { 'Down', 1 } },
      { key = 'LeftArrow',  mods = 'NONE', action = act.AdjustPaneSize { 'Left', 5 } },
      { key = 'RightArrow', mods = 'NONE', action = act.AdjustPaneSize { 'Right', 5 } },
      { key = 'UpArrow',    mods = 'NONE', action = act.AdjustPaneSize { 'Up', 5 } },
      { key = 'DownArrow',  mods = 'NONE', action = act.AdjustPaneSize { 'Down', 5 } },
    },

    search_mode = {
      { key = 'Enter',     mods = 'NONE', action = act.CopyMode 'PriorMatch' },
      { key = 'Escape',    mods = 'NONE', action = act.CopyMode 'Close' },
      { key = 'n',         mods = 'CTRL', action = act.CopyMode 'NextMatch' },
      { key = 'p',         mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
      { key = 'r',         mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
      { key = 'u',         mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
      { key = 'PageUp',    mods = 'NONE', action = act.CopyMode 'PriorMatchPage' },
      { key = 'PageDown',  mods = 'NONE', action = act.CopyMode 'NextMatchPage' },
      { key = 'UpArrow',   mods = 'NONE', action = act.CopyMode 'PriorMatch' },
      { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'NextMatch' },
    },

    tab = {
      { key = 'Escape', mods = 'NONE', action = act.PopKeyTable },
      { key = '1',      mods = 'NONE', action = act.ActivateTab(0) },
      { key = '2',      mods = 'NONE', action = act.ActivateTab(1) },
      { key = '3',      mods = 'NONE', action = act.ActivateTab(2) },
      { key = '4',      mods = 'NONE', action = act.ActivateTab(3) },
      { key = '5',      mods = 'NONE', action = act.ActivateTab(4) },
      { key = '6',      mods = 'NONE', action = act.ActivateTab(5) },
      { key = '7',      mods = 'NONE', action = act.ActivateTab(6) },
      { key = '8',      mods = 'NONE', action = act.ActivateTab(7) },
      { key = '9',      mods = 'NONE', action = act.ActivateTab(8) },
      { key = 'n',      mods = 'NONE', action = act.SpawnTab 'CurrentPaneDomain' },
    },
  },
  leader = { key = 'g', mods = "ALT" },
  window_frame = {
    font = wezterm.font('Source Code Pro', { weight = 'Bold' }),
    font_size = 10.0,

    active_titlebar_bg = '#3a464c',
    inactive_titlebar_bg = '#333c43',
  },
}

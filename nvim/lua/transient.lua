local M = {}

local floats = {}

local function dimensions(width_ratio, height_ratio)
  local available_height = vim.o.lines - vim.o.cmdheight
  local width = math.max(4, math.floor(vim.o.columns * width_ratio))
  local height = math.max(4, math.floor(available_height * height_ratio))
  return {
    width = math.min(width, vim.o.columns),
    height = math.min(height, available_height),
  }
end

local function configs(spec)
  local size = dimensions(spec.width, spec.height)
  local col = math.floor((vim.o.columns - size.width) / 2)
  local row = math.max(0, math.floor((vim.o.lines - vim.o.cmdheight - size.height) / 2))
  return {
    relative = "editor",
    width = size.width,
    height = size.height,
    col = col,
    row = row,
    style = "minimal",
    border = "none",
    focusable = false,
    zindex = 49,
  }, {
    relative = "editor",
    width = size.width - 2,
    height = size.height - 2,
    col = col + 1,
    row = row + 1,
    style = "minimal",
    border = "none",
    zindex = 50,
  }, size
end

local function draw_frame(buffer, size)
  local lines = { "╭" .. string.rep("─", size.width - 2) .. "╮" }
  for _ = 1, size.height - 2 do
    table.insert(lines, "│" .. string.rep(" ", size.width - 2) .. "│")
  end
  table.insert(lines, "╰" .. string.rep("─", size.width - 2) .. "╯")
  vim.bo[buffer].modifiable = true
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  vim.bo[buffer].modifiable = false
end

function M.open_window(buffer, options)
  options = options or {}
  local spec = {
    width = options.width or 0.9,
    height = options.height or 0.7,
  }
  local frame_config, content_config, size = configs(spec)
  local frame_buffer = vim.api.nvim_create_buf(false, true)
  vim.bo[frame_buffer].buftype = "nofile"
  vim.bo[frame_buffer].bufhidden = "wipe"
  vim.bo[frame_buffer].swapfile = false
  draw_frame(frame_buffer, size)

  local frame_window = vim.api.nvim_open_win(frame_buffer, false, frame_config)
  vim.wo[frame_window].winhighlight = "Normal:Normal"
  local window = vim.api.nvim_open_win(buffer, true, content_config)
  floats[window] = {
    width = spec.width,
    height = spec.height,
    frame_buffer = frame_buffer,
    frame_window = frame_window,
  }
  return window
end

function M.close_window(window)
  local spec = floats[window]
  floats[window] = nil
  if window and vim.api.nvim_win_is_valid(window) then
    vim.api.nvim_win_close(window, true)
  end
  if spec and vim.api.nvim_win_is_valid(spec.frame_window) then
    vim.api.nvim_win_close(spec.frame_window, true)
  end
end

function M.forget_window(window)
  local spec = floats[window]
  floats[window] = nil
  if spec and vim.api.nvim_win_is_valid(spec.frame_window) then
    vim.api.nvim_win_close(spec.frame_window, true)
  end
end

function M.bind(buffer, actions)
  for _, action in ipairs(actions) do
    vim.keymap.set(action.mode or "n", action.key, action.callback, {
      buffer = buffer,
      silent = action.silent ~= false,
      desc = action.desc or action.label,
    })
  end
end

function M.hint(actions)
  local hints = {}
  for _, action in ipairs(actions) do
    if action.hint ~= false then
      table.insert(hints, (action.display or action.key) .. " " .. action.label)
    end
  end
  return table.concat(hints, "   ")
end

vim.api.nvim_create_autocmd("VimResized", {
  callback = function()
    for window, spec in pairs(floats) do
      if vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_is_valid(spec.frame_window) then
        local frame_config, content_config, size = configs(spec)
        draw_frame(spec.frame_buffer, size)
        vim.api.nvim_win_set_config(spec.frame_window, frame_config)
        vim.api.nvim_win_set_config(window, content_config)
      else
        M.forget_window(window)
      end
    end
  end,
})

return M

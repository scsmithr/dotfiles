local M = {}

local namespace = vim.api.nvim_create_namespace("magit-light")
local states = {}

local function notify(message, level)
  vim.notify(message, level or vim.log.levels.INFO, { title = "Magit Light" })
end

local function run_git(root, args, callback)
  local command = { "git", "-C", root }
  vim.list_extend(command, args)

  vim.system(command, { text = true }, function(result)
    vim.schedule(function()
      callback(result)
    end)
  end)
end

local function git_error(result)
  local message = vim.trim(result.stderr or "")
  if message == "" then
    message = vim.trim(result.stdout or "")
  end
  notify(message ~= "" and message or "Git command failed", vim.log.levels.ERROR)
end

local function parse_status(output)
  local records = vim.split(output, "\0", { plain = true, trimempty = true })
  local branch = "unknown"
  local entries = {}
  local index = 1

  if records[1] and records[1]:sub(1, 3) == "## " then
    branch = records[1]:sub(4)
    index = 2
  end

  while index <= #records do
    local record = records[index]
    local status = record:sub(1, 2)
    local path = record:sub(4)
    local original_path

    if status:find("[RC]") then
      original_path = records[index + 1]
      index = index + 1
    end

    table.insert(entries, {
      status = status,
      index_status = status:sub(1, 1),
      worktree_status = status:sub(2, 2),
      path = path,
      original_path = original_path,
    })
    index = index + 1
  end

  return branch, entries
end

local function display_path(entry)
  local path = entry.path:gsub("\n", "\\n")
  if entry.original_path then
    return entry.original_path:gsub("\n", "\\n") .. " -> " .. path
  end
  return path
end

local function classify(entries)
  local sections = {
    conflicts = {},
    untracked = {},
    unstaged = {},
    staged = {},
  }

  for _, entry in ipairs(entries) do
    local x = entry.index_status
    local y = entry.worktree_status
    local conflict = x == "U" or y == "U" or entry.status == "AA" or entry.status == "DD"

    if conflict then
      table.insert(sections.conflicts, entry)
    elseif entry.status == "??" then
      table.insert(sections.untracked, entry)
    else
      if y ~= " " then
        table.insert(sections.unstaged, entry)
      end
      if x ~= " " then
        table.insert(sections.staged, entry)
      end
    end
  end

  return sections
end

local function add_highlight(buffer, group, line)
  vim.api.nvim_buf_add_highlight(buffer, namespace, group, line - 1, 0, -1)
end

local function render(buffer, branch, entries, focus_path)
  local state = states[buffer]
  if not state or not vim.api.nvim_buf_is_valid(buffer) then
    return
  end

  local sections = classify(entries)
  local lines = {
    "Magit Light  " .. state.root,
    "Branch: " .. branch,
    "",
    "s stage   u unstage   d diff   <Enter> visit   c commit   g refresh   q quit",
    "",
  }
  local line_items = {}
  local section_lines = {}
  local focus_line

  local function add_section(title, kind, items, marker)
    if #items == 0 then
      return
    end

    table.insert(lines, title)
    table.insert(section_lines, #lines)

    for _, entry in ipairs(items) do
      table.insert(lines, string.format("  %-2s %s", marker(entry), display_path(entry)))
      line_items[#lines] = {
        section = kind,
        path = entry.path,
        original_path = entry.original_path,
      }
      if focus_path and entry.path == focus_path and not focus_line then
        focus_line = #lines
      end
    end
    table.insert(lines, "")
  end

  add_section("Conflicts", "conflict", sections.conflicts, function(entry)
    return entry.status
  end)
  add_section("Untracked files", "untracked", sections.untracked, function()
    return "?"
  end)
  add_section("Unstaged changes", "unstaged", sections.unstaged, function(entry)
    return entry.worktree_status
  end)
  add_section("Staged changes", "staged", sections.staged, function(entry)
    return entry.index_status
  end)

  if #entries == 0 then
    table.insert(lines, "Working tree clean")
  end

  state.line_items = line_items
  vim.bo[buffer].modifiable = true
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  vim.api.nvim_buf_clear_namespace(buffer, namespace, 0, -1)
  add_highlight(buffer, "Title", 1)
  add_highlight(buffer, "Identifier", 2)
  add_highlight(buffer, "Comment", 4)
  for _, line in ipairs(section_lines) do
    add_highlight(buffer, "Statement", line)
  end
  vim.bo[buffer].modifiable = false
  vim.bo[buffer].modified = false

  local window = vim.fn.bufwinid(buffer)
  if window ~= -1 then
    vim.api.nvim_win_set_cursor(window, { focus_line or 1, 0 })
  end
end

local function refresh(buffer, focus_path)
  local state = states[buffer]
  if not state then
    return
  end

  run_git(state.root, { "status", "--porcelain=v1", "-z", "--branch", "--untracked-files=all" }, function(result)
    if result.code ~= 0 then
      git_error(result)
      return
    end
    local branch, entries = parse_status(result.stdout or "")
    render(buffer, branch, entries, focus_path)
  end)
end

local function item_under_cursor(buffer)
  local state = states[buffer]
  return state and state.line_items[vim.api.nvim_win_get_cursor(0)[1]] or nil
end

local function item_paths(item)
  local paths = { item.path }
  if item.original_path then
    table.insert(paths, item.original_path)
  end
  return paths
end

local function stage(buffer)
  local state = states[buffer]
  local item = item_under_cursor(buffer)
  if not item then
    notify("Move the cursor onto a file first")
    return
  end
  if item.section == "staged" then
    notify("That change is already staged")
    return
  end

  local args = { "add", "--" }
  vim.list_extend(args, item_paths(item))
  run_git(state.root, args, function(result)
    if result.code ~= 0 then
      git_error(result)
      return
    end
    refresh(buffer, item.path)
  end)
end

local function unstage(buffer)
  local state = states[buffer]
  local item = item_under_cursor(buffer)
  if not item then
    notify("Move the cursor onto a file first")
    return
  end
  if item.section ~= "staged" then
    notify("Move the cursor onto a staged change")
    return
  end

  run_git(state.root, { "rev-parse", "--verify", "HEAD" }, function(head_result)
    local args
    if head_result.code == 0 then
      args = { "restore", "--staged", "--" }
    else
      args = { "rm", "--cached", "--" }
    end
    vim.list_extend(args, item_paths(item))

    run_git(state.root, args, function(result)
      if result.code ~= 0 then
        git_error(result)
        return
      end
      refresh(buffer, item.path)
    end)
  end)
end

local function show_diff(buffer)
  local state = states[buffer]
  local item = item_under_cursor(buffer)
  if not item then
    notify("Move the cursor onto a file first")
    return
  end

  local args
  if item.section == "staged" then
    args = { "diff", "--cached", "--no-ext-diff", "--no-color", "--", item.path }
  elseif item.section == "untracked" then
    args = {
      "diff",
      "--no-index",
      "--no-color",
      "--",
      "/dev/null",
      vim.fs.joinpath(state.root, item.path),
    }
  else
    args = { "diff", "--no-ext-diff", "--no-color", "--", item.path }
  end

  run_git(state.root, args, function(result)
    if result.code > 1 then
      git_error(result)
      return
    end

    local output = result.stdout or ""
    if output == "" then
      notify("No diff for " .. item.path)
      return
    end

    vim.cmd("botright new")
    local diff_buffer = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_name(diff_buffer, "magit-diff://" .. diff_buffer .. "/" .. item.path)
    vim.bo[diff_buffer].buftype = "nofile"
    vim.bo[diff_buffer].bufhidden = "wipe"
    vim.bo[diff_buffer].swapfile = false
    vim.bo[diff_buffer].filetype = "diff"
    vim.api.nvim_buf_set_lines(diff_buffer, 0, -1, false, vim.split(output, "\n", { plain = true }))
    vim.bo[diff_buffer].modifiable = false
    vim.bo[diff_buffer].modified = false
    vim.keymap.set("n", "q", "<Cmd>close<CR>", { buffer = diff_buffer, silent = true })
  end)
end

local function visit_file(buffer)
  local state = states[buffer]
  local item = item_under_cursor(buffer)
  if not item then
    return
  end
  vim.cmd("tabnew " .. vim.fn.fnameescape(vim.fs.joinpath(state.root, item.path)))
end

local function commit(buffer)
  local state = states[buffer]
  vim.ui.input({ prompt = "Commit message: " }, function(message)
    if not message or vim.trim(message) == "" then
      return
    end

    run_git(state.root, { "commit", "-m", message }, function(result)
      if result.code ~= 0 then
        git_error(result)
        return
      end
      notify(vim.trim(result.stdout or "Commit created"))
      refresh(buffer)
    end)
  end)
end

local function close_status()
  if #vim.api.nvim_tabpage_list_wins(0) == 1 and #vim.api.nvim_list_tabpages() > 1 then
    vim.cmd.tabclose()
  else
    vim.cmd.close()
  end
end

local function repository_root()
  local name = vim.api.nvim_buf_get_name(0)
  local start = name ~= "" and vim.fs.dirname(name) or vim.fn.getcwd()
  local result = vim.system({ "git", "-C", start, "rev-parse", "--show-toplevel" }, { text = true }):wait()
  if result.code ~= 0 then
    return nil
  end
  return vim.trim(result.stdout or "")
end

function M.open()
  local current_buffer = vim.api.nvim_get_current_buf()
  if states[current_buffer] then
    refresh(current_buffer)
    return
  end

  local root = repository_root()
  if not root or root == "" then
    notify("Not inside a Git repository", vim.log.levels.ERROR)
    return
  end

  for buffer, state in pairs(states) do
    if state.root == root and vim.api.nvim_buf_is_valid(buffer) then
      local windows = vim.fn.win_findbuf(buffer)
      if windows[1] then
        vim.api.nvim_set_current_win(windows[1])
        refresh(buffer)
        return
      end
    end
  end

  vim.cmd.tabnew()
  local buffer = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_name(buffer, "magit://" .. root)
  vim.bo[buffer].buftype = "nofile"
  vim.bo[buffer].bufhidden = "wipe"
  vim.bo[buffer].buflisted = false
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].filetype = "magitlight"
  states[buffer] = { root = root, line_items = {} }

  vim.keymap.set("n", "g", function()
    refresh(buffer)
  end, { buffer = buffer, silent = true, desc = "Refresh" })
  vim.keymap.set("n", "s", function()
    stage(buffer)
  end, { buffer = buffer, silent = true, desc = "Stage file" })
  vim.keymap.set("n", "u", function()
    unstage(buffer)
  end, { buffer = buffer, silent = true, desc = "Unstage file" })
  vim.keymap.set("n", "d", function()
    show_diff(buffer)
  end, { buffer = buffer, silent = true, desc = "Show diff" })
  vim.keymap.set("n", "<CR>", function()
    visit_file(buffer)
  end, { buffer = buffer, silent = true, desc = "Visit file" })
  vim.keymap.set("n", "c", function()
    commit(buffer)
  end, { buffer = buffer, silent = true, desc = "Commit" })
  vim.keymap.set("n", "q", close_status, { buffer = buffer, silent = true, desc = "Close" })

  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buffer,
    once = true,
    callback = function()
      states[buffer] = nil
    end,
  })

  refresh(buffer)
end

return M

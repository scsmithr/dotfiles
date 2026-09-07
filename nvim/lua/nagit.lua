local M = {}

local transient = require("transient")
local namespace = vim.api.nvim_create_namespace("nagit")
local states = {}

local function notify(message, level)
  vim.notify(message, level or vim.log.levels.INFO, { title = "Nagit" })
end

local function run_git(root, args, callback, stdin)
  local command = { "git", "-C", root }
  vim.list_extend(command, args)
  local options = { text = true }
  if stdin then
    options.stdin = stdin
  end
  vim.system(command, options, function(result)
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
  local sections = { conflicts = {}, untracked = {}, unstaged = {}, staged = {} }
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

local function item_key(item)
  return item.section .. "\0" .. item.path
end

local function diff_args(item)
  if item.section == "staged" then
    return { "diff", "--cached", "--no-ext-diff", "--no-color", "--", item.path }
  elseif item.section == "untracked" then
    return { "diff", "--no-index", "--no-color", "--", "/dev/null", item.path }
  end
  return { "diff", "--no-ext-diff", "--no-color", "--", item.path }
end

local function parse_diff(output)
  local diff = { header = {}, hunks = {} }
  local hunk
  for _, line in ipairs(vim.split(output, "\n", { plain = true, trimempty = true })) do
    local old_start, old_count, new_start, new_count = line:match("^@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@")
    if old_start then
      hunk = {
        header = line,
        lines = {},
        old_start = tonumber(old_start),
        old_count = tonumber(old_count ~= "" and old_count or "1"),
        new_start = tonumber(new_start),
        new_count = tonumber(new_count ~= "" and new_count or "1"),
      }
      table.insert(diff.hunks, hunk)
    elseif hunk then
      table.insert(hunk.lines, line)
    elseif line ~= "" then
      table.insert(diff.header, line)
    end
  end
  return diff
end

local function current_focus(buffer)
  local state = states[buffer]
  local window = vim.fn.bufwinid(buffer)
  if not state or window == -1 then
    return nil
  end
  local item = state.line_items[vim.api.nvim_win_get_cursor(window)[1]]
  return item and { path = item.path, section = item.section } or nil
end

local function add_highlight(buffer, group, line)
  vim.api.nvim_buf_add_highlight(buffer, namespace, group, line - 1, 0, -1)
end

local function render(buffer, requested_focus)
  local state = states[buffer]
  if not state or not vim.api.nvim_buf_is_valid(buffer) then
    return
  end
  local focus = requested_focus or current_focus(buffer)
  local sections = classify(state.entries)
  local lines = {
    "Nagit  " .. state.root,
    "Branch: " .. state.branch,
    "",
    transient.hint(state.file_actions),
    transient.hint(state.repo_actions),
    "Visual: " .. transient.hint(state.visual_actions),
    "",
  }
  local line_items = {}
  local highlights = {
    { "Title", 1 },
    { "Identifier", 2 },
    { "Comment", 4 },
    { "Comment", 5 },
    { "Comment", 6 },
  }
  local focus_line

  local function add_line(text, item, highlight)
    table.insert(lines, text)
    if item then
      line_items[#lines] = item
    end
    if highlight then
      table.insert(highlights, { highlight, #lines })
    end
  end

  local function add_section(title, section, entries, marker)
    if #entries == 0 then
      return
    end
    add_line(title, nil, "Statement")
    for _, entry in ipairs(entries) do
      local file_item = {
        kind = "file",
        section = section,
        path = entry.path,
        original_path = entry.original_path,
      }
      local key = item_key(file_item)
      local indicator = state.expanded[key] and "▼" or "▶"
      add_line(string.format("  %s %-2s %s", indicator, marker(entry), display_path(entry)), file_item)
      if focus and focus.path == entry.path and focus.section == section and not focus_line then
        focus_line = #lines
      end
      if state.expanded[key] then
        local diff = state.diffs[key]
        if not diff then
          add_line("      Loading diff…", file_item, "Comment")
        elseif #diff.hunks == 0 then
          add_line("      (no textual diff)", file_item, "Comment")
        else
          for _, hunk in ipairs(diff.hunks) do
            local hunk_item = {
              kind = "hunk",
              section = section,
              path = entry.path,
              original_path = entry.original_path,
              diff = diff,
              hunk = hunk,
            }
            add_line("    " .. hunk.header, hunk_item, "DiffChange")
            for diff_line, line in ipairs(hunk.lines) do
              local line_item = vim.tbl_extend("force", hunk_item, { diff_line = diff_line })
              local highlight
              if line:sub(1, 1) == "+" then
                highlight = "DiffAdd"
              elseif line:sub(1, 1) == "-" then
                highlight = "DiffDelete"
              end
              add_line("    " .. line, line_item, highlight)
            end
          end
        end
      end
    end
    add_line("")
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
  if #state.entries == 0 then
    add_line("Working tree clean")
  end

  state.line_items = line_items
  vim.bo[buffer].modifiable = true
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  vim.api.nvim_buf_clear_namespace(buffer, namespace, 0, -1)
  for _, highlight in ipairs(highlights) do
    add_highlight(buffer, highlight[1], highlight[2])
  end
  vim.bo[buffer].modifiable = false
  vim.bo[buffer].modified = false
  local window = vim.fn.bufwinid(buffer)
  if window ~= -1 then
    vim.api.nvim_win_set_cursor(window, { focus_line or 1, 0 })
  end
end

local function load_diff(buffer, item, generation)
  local state = states[buffer]
  if not state then
    return
  end
  local key = item_key(item)
  run_git(state.root, diff_args(item), function(result)
    local current = states[buffer]
    if not current or current.generation ~= generation or not current.expanded[key] then
      return
    end
    if result.code > 1 then
      git_error(result)
      current.expanded[key] = nil
      render(buffer)
      return
    end
    current.diffs[key] = parse_diff(result.stdout or "")
    render(buffer, { path = item.path, section = item.section })
  end)
end

local function refresh(buffer, focus)
  local state = states[buffer]
  if not state then
    return
  end
  state.busy = true
  state.generation = state.generation + 1
  local generation = state.generation
  run_git(state.root, { "status", "--porcelain=v1", "-z", "--branch", "--untracked-files=all" }, function(result)
    local current = states[buffer]
    if not current or current.generation ~= generation then
      return
    end
    if result.code ~= 0 then
      current.busy = false
      git_error(result)
      return
    end
    current.branch, current.entries = parse_status(result.stdout or "")
    current.diffs = {}
    local valid = {}
    local sections = classify(current.entries)
    local section_order = { "conflicts", "untracked", "unstaged", "staged" }
    for _, section_name in ipairs(section_order) do
      local section = section_name == "conflicts" and "conflict" or section_name:gsub("s$", "")
      for _, entry in ipairs(sections[section_name]) do
        local item = { section = section, path = entry.path, original_path = entry.original_path }
        local key = item_key(item)
        valid[key] = true
        if current.expanded[key] then
          load_diff(buffer, item, generation)
        end
      end
    end
    for key in pairs(current.expanded) do
      if not valid[key] then
        current.expanded[key] = nil
      end
    end
    current.busy = false
    render(buffer, focus)
  end)
end

local function item_under_cursor(buffer)
  local state = states[buffer]
  return state and state.line_items[vim.api.nvim_win_get_cursor(0)[1]] or nil
end

local function selected_range(buffer)
  local mode = vim.api.nvim_get_mode().mode
  local first
  local last
  if mode == "v" or mode == "V" or mode == "\22" then
    first = vim.fn.getpos("v")[2]
    last = vim.api.nvim_win_get_cursor(0)[1]
  else
    first = vim.api.nvim_buf_get_mark(buffer, "<")[1]
    last = vim.api.nvim_buf_get_mark(buffer, ">")[1]
  end
  if first > last then
    first, last = last, first
  end
  return first, last
end

local function item_paths(item)
  local paths = { item.path }
  if item.original_path then
    table.insert(paths, item.original_path)
  end
  return paths
end

local function patch_for_hunk(diff, hunk)
  local lines = vim.list_slice(diff.header)
  table.insert(lines, hunk.header)
  vim.list_extend(lines, hunk.lines)
  return table.concat(lines, "\n") .. "\n"
end

local function selected_patch(diff, hunk, selected, compact_new_file)
  if compact_new_file then
    local selected_lines = {}
    for index, line in ipairs(hunk.lines) do
      if selected[index] and line:sub(1, 1) == "+" then
        table.insert(selected_lines, line)
      end
    end
    if #selected_lines == 0 then
      return nil
    end
    local lines = vim.list_slice(diff.header)
    table.insert(lines, string.format("@@ -0,0 +1,%d @@", #selected_lines))
    vim.list_extend(lines, selected_lines)
    return table.concat(lines, "\n") .. "\n"
  end

  local groups = {}
  local group
  local old_line = hunk.old_start
  local new_line = hunk.new_start
  local function finish_group()
    if group then
      table.insert(groups, group)
      group = nil
    end
  end
  for index, line in ipairs(hunk.lines) do
    local prefix = line:sub(1, 1)
    local changed = prefix == "+" or prefix == "-"
    if changed and selected[index] then
      if not group then
        group = { old_start = old_line, new_start = new_line, old_count = 0, new_count = 0, lines = {} }
      end
      table.insert(group.lines, line)
      if prefix == "-" then
        group.old_count = group.old_count + 1
      else
        group.new_count = group.new_count + 1
      end
    elseif prefix == "\\" and group then
      table.insert(group.lines, line)
    else
      finish_group()
    end
    if prefix == " " then
      old_line = old_line + 1
      new_line = new_line + 1
    elseif prefix == "-" then
      old_line = old_line + 1
    elseif prefix == "+" then
      new_line = new_line + 1
    end
  end
  finish_group()
  if #groups == 0 then
    return nil
  end
  local lines = vim.list_slice(diff.header)
  for _, selected_group in ipairs(groups) do
    local old_start = selected_group.old_start
    local new_start = selected_group.new_start
    if selected_group.old_count == 0 then
      old_start = math.max(0, old_start - 1)
    end
    if selected_group.new_count == 0 then
      new_start = math.max(0, new_start - 1)
    end
    table.insert(lines, string.format(
      "@@ -%d,%d +%d,%d @@",
      old_start,
      selected_group.old_count,
      new_start,
      selected_group.new_count
    ))
    vim.list_extend(lines, selected_group.lines)
  end
  return table.concat(lines, "\n") .. "\n"
end

local function untracked_discard_patch(diff, hunk, selected)
  local diff_header
  local new_header
  for _, line in ipairs(diff.header) do
    if line:match("^diff %-%-git ") then
      diff_header = line
    elseif line:match("^%+%+%+ ") then
      new_header = line
    end
  end
  if not diff_header or not new_header then
    return nil
  end

  local old_header = new_header:gsub("^%+%+%+ b/", "--- a/", 1)
  if old_header == new_header then
    old_header = new_header:gsub('^%+%+%+ "b/', '--- "a/', 1)
  end

  local groups = {}
  local group
  local new_line = hunk.new_start
  local function finish_group()
    if group then
      table.insert(groups, group)
      group = nil
    end
  end

  for index, line in ipairs(hunk.lines) do
    if line:sub(1, 1) == "+" and selected[index] then
      if not group then
        group = { new_start = new_line, lines = {} }
      end
      table.insert(group.lines, line)
    else
      finish_group()
    end
    if line:sub(1, 1) == "+" then
      new_line = new_line + 1
    end
  end
  finish_group()
  if #groups == 0 then
    return nil
  end

  local lines = { diff_header, old_header, new_header }
  local removed = 0
  for _, selected_group in ipairs(groups) do
    local old_start = selected_group.new_start - 1 - removed
    table.insert(lines, string.format(
      "@@ -%d,0 +%d,%d @@",
      old_start,
      selected_group.new_start,
      #selected_group.lines
    ))
    vim.list_extend(lines, selected_group.lines)
    removed = removed + #selected_group.lines
  end
  return table.concat(lines, "\n") .. "\n"
end

local function apply_to_index(buffer, item, patch, reverse)
  local state = states[buffer]
  if state.busy then
    notify("Git operation in progress")
    return
  end
  state.busy = true
  local args = { "apply", "--cached", "--unidiff-zero", "--recount" }
  if reverse then
    table.insert(args, "--reverse")
  end
  run_git(state.root, args, function(result)
    if result.code ~= 0 then
      state.busy = false
      git_error(result)
      return
    end
    refresh(buffer, { path = item.path, section = item.section })
  end, patch)
end

local function discard_patch(buffer, item, patch)
  local state = states[buffer]
  state.busy = true

  local function args(cached, check)
    local result = { "apply", "--reverse", "--unidiff-zero", "--recount" }
    if cached then
      table.insert(result, "--cached")
    end
    if check then
      table.insert(result, "--check")
    end
    return result
  end

  local function failed(result, message)
    state.busy = false
    if message then
      notify(message, vim.log.levels.ERROR)
    else
      git_error(result)
    end
  end

  local function apply_worktree()
    run_git(state.root, args(false, false), function(result)
      if result.code ~= 0 then
        failed(result)
        return
      end
      refresh(buffer, { path = item.path, section = item.section })
    end, patch)
  end

  if item.section ~= "staged" then
    run_git(state.root, args(false, true), function(result)
      if result.code ~= 0 then
        failed(result)
        return
      end
      apply_worktree()
    end, patch)
    return
  end

  run_git(state.root, args(true, true), function(index_check)
    if index_check.code ~= 0 then
      failed(index_check)
      return
    end
    run_git(state.root, args(false, true), function(worktree_check)
      if worktree_check.code ~= 0 then
        failed(worktree_check, "Cannot discard this staged change because it overlaps unstaged work")
        return
      end
      run_git(state.root, args(true, false), function(index_result)
        if index_result.code ~= 0 then
          failed(index_result)
          return
        end
        apply_worktree()
      end, patch)
    end, patch)
  end, patch)
end

local function stage(buffer)
  local state = states[buffer]
  if state.busy then
    notify("Git operation in progress")
    return
  end
  local item = item_under_cursor(buffer)
  if not item then
    notify("Move the cursor onto a file or hunk")
    return
  elseif item.section == "staged" then
    notify("That change is already staged")
    return
  elseif item.kind == "hunk" then
    apply_to_index(buffer, item, patch_for_hunk(item.diff, item.hunk), false)
    return
  end
  local args = { "add", "--" }
  vim.list_extend(args, item_paths(item))
  state.busy = true
  run_git(state.root, args, function(result)
    if result.code ~= 0 then
      state.busy = false
      git_error(result)
      return
    end
    refresh(buffer, { path = item.path, section = item.section })
  end)
end

local function unstage(buffer)
  local state = states[buffer]
  if state.busy then
    notify("Git operation in progress")
    return
  end
  local item = item_under_cursor(buffer)
  if not item then
    notify("Move the cursor onto a staged file or hunk")
    return
  elseif item.section ~= "staged" then
    notify("Move the cursor onto a staged change")
    return
  elseif item.kind == "hunk" then
    apply_to_index(buffer, item, patch_for_hunk(item.diff, item.hunk), true)
    return
  end
  state.busy = true
  run_git(state.root, { "rev-parse", "--verify", "HEAD" }, function(head_result)
    local args = head_result.code == 0 and { "restore", "--staged", "--" } or { "rm", "--cached", "--" }
    vim.list_extend(args, item_paths(item))
    run_git(state.root, args, function(result)
      if result.code ~= 0 then
        state.busy = false
        git_error(result)
        return
      end
      refresh(buffer, { path = item.path, section = item.section })
    end)
  end)
end

local function selection(buffer, reverse)
  local state = states[buffer]
  local first, last = selected_range(buffer)
  local hunk_item
  local selected = {}
  for line = first, last do
    local item = state.line_items[line]
    if item and item.kind == "hunk" then
      if hunk_item and hunk_item.hunk ~= item.hunk then
        notify("Select changed lines from one hunk at a time")
        return
      end
      hunk_item = hunk_item or item
      if item.diff_line then
        local prefix = item.hunk.lines[item.diff_line]:sub(1, 1)
        if prefix == "+" or prefix == "-" then
          selected[item.diff_line] = true
        end
      end
    end
  end
  if not hunk_item or vim.tbl_isempty(selected) then
    notify("Select one or more added or removed lines")
    return
  elseif reverse and hunk_item.section ~= "staged" then
    notify("Selected lines must be in a staged change")
    return
  elseif not reverse and hunk_item.section == "staged" then
    notify("Selected lines are already staged")
    return
  end
  local patch = selected_patch(hunk_item.diff, hunk_item.hunk, selected, hunk_item.section == "untracked")
  if patch then
    apply_to_index(buffer, hunk_item, patch, reverse)
  end
end

local function confirm_discard(prompt, callback)
  vim.ui.select({ "Discard", "Cancel" }, {
    prompt = prompt .. " This cannot be undone.",
  }, function(choice)
    if choice == "Discard" then
      callback()
    end
  end)
end

local function discard(buffer)
  local state = states[buffer]
  if state.busy then
    notify("Git operation in progress")
    return
  end
  local item = item_under_cursor(buffer)
  if not item then
    notify("Move the cursor onto a file or hunk")
    return
  end

  local scope
  if item.section == "untracked" and item.kind == "file" then
    scope = "the untracked file "
  elseif item.kind == "hunk" then
    scope = "this " .. item.section .. " hunk from "
  else
    scope = "all " .. item.section .. " changes in "
  end
  confirm_discard("Discard " .. scope .. item.path .. "?", function()
    if not states[buffer] or state.busy then
      return
    end
    if item.kind == "hunk" then
      discard_patch(buffer, item, patch_for_hunk(item.diff, item.hunk))
      return
    end

    state.busy = true
    if item.section == "untracked" then
      run_git(state.root, { "clean", "-f", "--", item.path }, function(result)
        if result.code ~= 0 then
          state.busy = false
          git_error(result)
          return
        end
        refresh(buffer)
      end)
    elseif item.section == "conflict" then
      local args = { "restore", "--source=HEAD", "--staged", "--worktree", "--" }
      vim.list_extend(args, item_paths(item))
      run_git(state.root, args, function(result)
        if result.code ~= 0 then
          state.busy = false
          git_error(result)
          return
        end
        refresh(buffer)
      end)
    elseif item.section == "staged" then
      run_git(state.root, diff_args(item), function(result)
        if result.code > 1 then
          state.busy = false
          git_error(result)
          return
        end
        local patch = result.stdout or ""
        if patch == "" then
          state.busy = false
          notify("No change to discard")
          return
        end
        discard_patch(buffer, item, patch)
      end)
    else
      local args = { "restore", "--worktree", "--" }
      vim.list_extend(args, item_paths(item))
      run_git(state.root, args, function(result)
        if result.code ~= 0 then
          state.busy = false
          git_error(result)
          return
        end
        refresh(buffer)
      end)
    end
  end)
end

local function discard_selection(buffer)
  local state = states[buffer]
  if state.busy then
    notify("Git operation in progress")
    return
  end
  local first, last = selected_range(buffer)

  local hunk_item
  local selected = {}
  for line = first, last do
    local item = state.line_items[line]
    if item and item.kind == "hunk" then
      if hunk_item and hunk_item.hunk ~= item.hunk then
        notify("Select changed lines from one hunk at a time")
        return
      end
      hunk_item = hunk_item or item
      if item.diff_line then
        local prefix = item.hunk.lines[item.diff_line]:sub(1, 1)
        if prefix == "+" or prefix == "-" then
          selected[item.diff_line] = true
        end
      end
    end
  end
  if not hunk_item or vim.tbl_isempty(selected) then
    notify("Select one or more added or removed lines")
    return
  end

  local patch
  if hunk_item.section == "untracked" then
    patch = untracked_discard_patch(hunk_item.diff, hunk_item.hunk, selected)
  else
    patch = selected_patch(hunk_item.diff, hunk_item.hunk, selected, false)
  end
  if not patch then
    notify("Could not build a patch for that selection", vim.log.levels.ERROR)
    return
  end

  confirm_discard(
    "Discard the selected " .. hunk_item.section .. " lines from " .. hunk_item.path .. "?",
    function()
      if states[buffer] and not state.busy then
        discard_patch(buffer, hunk_item, patch)
      end
    end
  )
end

local function toggle_diff(buffer)
  local state = states[buffer]
  if state.busy then
    notify("Git operation in progress")
    return
  end
  local item = item_under_cursor(buffer)
  if not item then
    notify("Move the cursor onto a file")
    return
  end
  local key = item_key(item)
  if state.expanded[key] then
    state.expanded[key] = nil
    state.diffs[key] = nil
    render(buffer, { path = item.path, section = item.section })
  else
    state.expanded[key] = true
    render(buffer, { path = item.path, section = item.section })
    load_diff(buffer, item, state.generation)
  end
end

local function visit_file(buffer)
  local state = states[buffer]
  local item = item_under_cursor(buffer)
  if item then
    vim.cmd("tabnew " .. vim.fn.fnameescape(vim.fs.joinpath(state.root, item.path)))
  end
end

local function show_log(status_buffer)
  local state = states[status_buffer]
  if state.log_window and vim.api.nvim_win_is_valid(state.log_window) then
    vim.api.nvim_set_current_win(state.log_window)
    return
  end

  run_git(state.root, { "log", "--graph", "--decorate", "--oneline", "--all", "--max-count=200" }, function(result)
    state = states[status_buffer]
    if not state then
      return
    end
    if result.code ~= 0 then
      git_error(result)
      return
    end
    local buffer = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buffer, "nagit-log://" .. buffer)
    vim.bo[buffer].buftype = "nofile"
    vim.bo[buffer].bufhidden = "wipe"
    vim.bo[buffer].buflisted = false
    vim.bo[buffer].swapfile = false
    vim.bo[buffer].filetype = "git"
    local output = vim.trim(result.stdout or "")
    local lines = output == "" and { "(no commits)" } or vim.split(output, "\n", { plain = true })
    vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
    vim.bo[buffer].modifiable = false
    vim.bo[buffer].modified = false

    local window = transient.open_window(buffer, { width = 0.9, height = 0.82 })
    state.log_buffer = buffer
    state.log_window = window
    vim.wo[window].number = false
    vim.wo[window].relativenumber = false
    vim.wo[window].signcolumn = "no"
    transient.bind(buffer, {
      {
        key = "q",
        label = "quit",
        callback = function()
          transient.close_window(window)
        end,
      },
    })
    vim.api.nvim_create_autocmd("BufWipeout", {
      buffer = buffer,
      once = true,
      callback = function()
        transient.forget_window(window)
        if states[status_buffer] then
          states[status_buffer].log_buffer = nil
          states[status_buffer].log_window = nil
        end
      end,
    })
  end)
end

local function push(buffer)
  local state = states[buffer]
  if state.busy then
    notify("Git operation in progress")
    return
  end
  state.busy = true
  run_git(state.root, { "push" }, function(result)
    state.busy = false
    if result.code ~= 0 then
      git_error(result)
      return
    end
    local message = vim.trim((result.stdout or "") .. "\n" .. (result.stderr or ""))
    notify(message ~= "" and message or "Push complete")
    refresh(buffer)
  end)
end

local close_window = transient.close_window

local function open_commit(status_buffer)
  local state = states[status_buffer]
  if state.commit_window and vim.api.nvim_win_is_valid(state.commit_window) then
    vim.api.nvim_set_current_win(state.commit_window)
    return
  end
  local buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buffer, "nagit-commit://" .. buffer)
  vim.bo[buffer].buftype = "acwrite"
  vim.bo[buffer].bufhidden = "wipe"
  vim.bo[buffer].buflisted = false
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].filetype = "gitcommit"
  local lines = {
    "",
    "# Write a commit message. Save with :write to commit; q cancels.",
    "#",
    "# Branch: " .. state.branch,
    "#",
    "# Staged changes:",
  }
  local staged = classify(state.entries).staged
  if #staged == 0 then
    table.insert(lines, "#   (none)")
  else
    for _, entry in ipairs(staged) do
      table.insert(lines, "#   " .. entry.index_status .. " " .. display_path(entry))
    end
  end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  vim.bo[buffer].modified = false
  local window = transient.open_window(buffer, { width = 0.78, height = 0.65 })
  state.commit_buffer = buffer
  state.commit_window = window
  vim.wo[window].number = false
  vim.wo[window].relativenumber = false
  vim.wo[window].signcolumn = "no"
  vim.wo[window].wrap = true
  vim.wo[window].spell = true
  vim.api.nvim_win_set_cursor(window, { 1, 0 })

  local function cancel()
    if vim.bo[buffer].modified then
      local choice = vim.fn.confirm("Discard this commit message?", "&Discard\n&Keep editing", 2)
      if choice ~= 1 then
        return
      end
    end
    close_window(window)
  end
  transient.bind(buffer, {
    { key = "q", label = "cancel", desc = "Cancel commit", callback = cancel },
  })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    buffer = buffer,
    callback = function()
      if not vim.api.nvim_buf_is_valid(buffer) or vim.b[buffer].committing then
        return
      end
      local message_lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
      local meaningful = {}
      for _, line in ipairs(message_lines) do
        if not line:match("^%s*#") then
          table.insert(meaningful, line)
        end
      end
      if vim.trim(table.concat(meaningful, "\n")) == "" then
        notify("Commit message is empty", vim.log.levels.ERROR)
        return
      end
      vim.b[buffer].committing = true
      vim.bo[buffer].modifiable = false
      run_git(state.root, { "commit", "--cleanup=strip", "-F", "-" }, function(result)
        if not vim.api.nvim_buf_is_valid(buffer) then
          return
        end
        vim.b[buffer].committing = false
        if result.code ~= 0 then
          vim.bo[buffer].modifiable = true
          git_error(result)
          return
        end
        vim.bo[buffer].modified = false
        close_window(window)
        notify(vim.trim(result.stdout or "Commit created"))
        refresh(status_buffer)
      end, table.concat(message_lines, "\n") .. "\n")
    end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buffer,
    once = true,
    callback = function()
      if states[status_buffer] then
        states[status_buffer].commit_buffer = nil
        states[status_buffer].commit_window = nil
      end
    end,
  })
end

local function close_status(buffer)
  local state = states[buffer]
  if state then
    close_window(state.commit_window)
    close_window(state.log_window)
    close_window(state.window)
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

  local buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buffer, "nagit://" .. root)
  vim.bo[buffer].buftype = "nofile"
  vim.bo[buffer].bufhidden = "wipe"
  vim.bo[buffer].buflisted = false
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].filetype = "nagit"
  local window = transient.open_window(buffer, { width = 0.9, height = 0.7 })
  vim.wo[window].cursorline = true
  vim.wo[window].number = false
  vim.wo[window].relativenumber = false
  vim.wo[window].signcolumn = "no"
  states[buffer] = {
    root = root,
    branch = "unknown",
    entries = {},
    line_items = {},
    expanded = {},
    diffs = {},
    generation = 0,
    busy = false,
    window = window,
  }

  local state = states[buffer]
  state.file_actions = {
    {
      key = "<Tab>",
      display = "<Tab>",
      label = "diff",
      desc = "Toggle diff",
      callback = function()
        toggle_diff(buffer)
      end,
    },
    {
      key = "s",
      label = "stage",
      desc = "Stage file or hunk",
      callback = function()
        stage(buffer)
      end,
    },
    {
      key = "u",
      label = "unstage",
      desc = "Unstage file or hunk",
      callback = function()
        unstage(buffer)
      end,
    },
    {
      key = "x",
      label = "discard",
      desc = "Discard file or hunk",
      callback = function()
        discard(buffer)
      end,
    },
    {
      key = "<CR>",
      display = "<Enter>",
      label = "visit",
      desc = "Visit file",
      callback = function()
        visit_file(buffer)
      end,
    },
  }
  state.repo_actions = {
    {
      key = "c",
      label = "commit",
      callback = function()
        open_commit(buffer)
      end,
    },
    {
      key = "l",
      label = "log",
      callback = function()
        show_log(buffer)
      end,
    },
    {
      key = "p",
      label = "push",
      callback = function()
        push(buffer)
      end,
    },
    {
      key = "g",
      label = "refresh",
      callback = function()
        refresh(buffer)
      end,
    },
    {
      key = "q",
      label = "quit",
      callback = function()
        close_status(buffer)
      end,
    },
  }
  state.visual_actions = {
    {
      mode = "x",
      key = "s",
      label = "stage",
      desc = "Stage selected lines",
      callback = function()
        selection(buffer, false)
      end,
    },
    {
      mode = "x",
      key = "u",
      label = "unstage",
      desc = "Unstage selected lines",
      callback = function()
        selection(buffer, true)
      end,
    },
    {
      mode = "x",
      key = "x",
      label = "discard",
      desc = "Discard selected lines",
      callback = function()
        discard_selection(buffer)
      end,
    },
  }
  transient.bind(buffer, state.file_actions)
  transient.bind(buffer, state.repo_actions)
  transient.bind(buffer, state.visual_actions)
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buffer,
    once = true,
    callback = function()
      transient.forget_window(window)
      states[buffer] = nil
    end,
  })
  refresh(buffer)
end

return M

vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Use terminal profile colors.
vim.opt.termguicolors = false

-- Markdown code defaults to Comment; use the theme's blue instead.
vim.api.nvim_create_autocmd("ColorScheme", {
  group = vim.api.nvim_create_augroup("MarkdownCodeColors", { clear = true }),
  callback = function()
    vim.api.nvim_set_hl(0, "@markup.raw.markdown_inline", { link = "Statement" })
    vim.api.nvim_set_hl(0, "@markup.raw.block.markdown", { link = "Statement" })
  end,
})

-- Use the regular comment style for Rust line and block doc comments.
vim.api.nvim_create_autocmd("ColorScheme", {
  group = vim.api.nvim_create_augroup("RustDocColors", { clear = true }),
  callback = function()
    vim.api.nvim_set_hl(0, "rustCommentLineDoc", { link = "Comment" })
  end,
})
vim.cmd.colorscheme("wildcharm")

-- Lunaperche gives the completion menu and its border a grey background.
-- Keep its foreground colors, but let the terminal background show through.
vim.cmd([[
  highlight Pmenu ctermbg=NONE guibg=NONE
  highlight PmenuMatch ctermbg=NONE guibg=NONE
  highlight PmenuKind ctermbg=NONE guibg=NONE
  highlight PmenuExtra ctermbg=NONE guibg=NONE
  highlight PmenuBorder ctermbg=NONE guibg=NONE
]])
vim.api.nvim_set_hl(0, "FloatBorder", { link = "PmenuBorder" })

vim.opt.fillchars:append({ vert = "│" })
vim.cmd("highlight WinSeparator ctermbg=NONE guibg=NONE")

vim.opt.pumborder = "rounded"
vim.opt.winborder = "rounded"

vim.opt.number = true
vim.opt.signcolumn = "yes"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.scrolloff = 2

vim.opt.wrap = false
vim.opt.textwidth = 80

-- System clipboard
vim.opt.clipboard = "unnamedplus"

vim.g.netrw_banner = 0

local function project_root()
  return vim.fs.root(0, { ".git" })
    or vim.fs.root(0, {
      "go.work",
      "go.mod",
      "Cargo.toml",
      "package.json",
      "pyproject.toml",
      "CMakeLists.txt",
      "flake.nix",
    })
    or vim.fn.getcwd()
end

local function fuzzy_find()
  vim.fn["fzf#run"]({
    dir = project_root(),
    source = "rg --files",
    sink = function(path)
      vim.cmd.edit(vim.fn.fnameescape(path))
    end,
    options = { "--layout", "reverse", "--prompt", "Files> ", "--scheme", "path" },
    window = { width = 0.9, height = 0.6 },
  })
end

local function fuzzy_grep()
  local reload = table.concat({
    "query={q};",
    "if [ ${#query} -lt 3 ]; then",
    "  true;",
    "else",
    "  rg --column --line-number --no-heading --color=never --smart-case -- \"$query\" || true;",
    "fi",
  }, " ")

  vim.fn["fzf#run"]({
    dir = project_root(),
    source = "true",
    sink = function(match)
      local path, line, column = match:match("^(.-):(%d+):(%d+):")
      if not path then
        return
      end

      vim.cmd.edit(vim.fn.fnameescape(path))
      vim.api.nvim_win_set_cursor(0, { tonumber(line), tonumber(column) - 1 })
    end,
    options = {
      "--disabled",
      "--layout", "reverse",
      "--delimiter", ":",
      "--nth", "4..",
      "--prompt", "Grep> ",
      "--header", "Type at least 3 characters",
      "--bind", "change:reload(" .. reload .. ")",
    },
    window = { width = 0.9, height = 0.6 },
  })
end

vim.keymap.set("n", "<C-p>", fuzzy_find, { desc = "Fuzzy Find Files" })
vim.keymap.set("n", "<leader>p", fuzzy_find, { desc = "Fuzzy Find Files" })
vim.keymap.set("n", "<leader>g", fuzzy_grep, { desc = "Fuzzy Grep Project" })
vim.keymap.set("n", "<leader>e", function()
  vim.cmd("Explore " .. vim.fn.fnameescape(project_root()))
end, { desc = "Explore Project" })

vim.keymap.set("n", "<leader>d", function()
  local path = vim.api.nvim_buf_get_name(0)
  local directory

  if path == "" then
    directory = vim.fn.getcwd()
  elseif vim.fn.isdirectory(path) == 1 then
    directory = path
  else
    directory = vim.fs.dirname(path)
  end

  vim.cmd("Explore " .. vim.fn.fnameescape(directory))
end, { desc = "Explore Buffer Directory" })

local function find_project_terminal(root)
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buffer) and vim.b[buffer].project_terminal_root == root then
      local job = vim.b[buffer].terminal_job_id
      if type(job) == "number" and vim.fn.jobwait({ job }, 0)[1] == -1 then
        return buffer
      end
    end
  end
end

local function open_terminal_float(buffer, root)
  for _, window in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
    if vim.api.nvim_win_get_buf(window) == buffer then
      vim.api.nvim_set_current_win(window)
      return
    end
  end

  local width = math.floor(vim.o.columns * 0.85)
  local height = math.floor((vim.o.lines - vim.o.cmdheight) * 0.8)

  vim.api.nvim_open_win(buffer, true, {
    relative = "editor",
    row = math.max(0, math.floor((vim.o.lines - height) / 2) - 1),
    col = math.floor((vim.o.columns - width) / 2),
    width = width,
    height = height,
    style = "minimal",
    border = "rounded",
    title = " Terminal: " .. vim.fs.basename(root) .. " ",
    title_pos = "center",
  })
end

vim.keymap.set("n", "<leader>t", function()
  local root = vim.b.project_terminal_root or project_root()

  local terminal = find_project_terminal(root)
  if terminal then
    open_terminal_float(terminal, root)
    vim.cmd.startinsert()
    return
  end

  terminal = vim.api.nvim_create_buf(false, true)
  open_terminal_float(terminal, root)
  vim.b[terminal].project_terminal_root = root
  vim.fn.jobstart({ vim.o.shell }, {
    cwd = root,
    term = true,
  })
  vim.cmd.startinsert()
end, { desc = "Terminal at Project Root" })

vim.keymap.set("t", "<Esc><Esc>", [[<C-\><C-n>]], {
  desc = "Exit Terminal Mode",
})

local language_servers = {
  rust_analyzer = {
    cmd = { "rust-analyzer" },
    filetypes = { "rust" },
    root_markers = { "Cargo.toml", "rust-project.json", ".git" },
  },
  gopls = {
    cmd = { "gopls" },
    filetypes = { "go", "gomod", "gowork", "gotmpl" },
    root_markers = { "go.work", "go.mod", ".git" },
  },
  ts_ls = {
    cmd = { "typescript-language-server", "--stdio" },
    filetypes = {
      "javascript",
      "javascriptreact",
      "typescript",
      "typescriptreact",
    },
    root_markers = { "tsconfig.json", "jsconfig.json", "package.json", ".git" },
  },
  clangd = {
    cmd = { "clangd" },
    filetypes = { "c", "cpp", "objc", "objcpp" },
    root_markers = {
      ".clangd",
      "compile_commands.json",
      "compile_flags.txt",
      ".git",
    },
  },
  sourcekit = {
    cmd = { "sourcekit-lsp" },
    filetypes = { "swift" },
    root_dir = function(bufnr, on_dir)
      on_dir(vim.fs.root(bufnr, {
        { "buildServer.json", ".bsp" },
        function(name)
          return vim.endswith(name, ".xcodeproj")
            or vim.endswith(name, ".xcworkspace")
        end,
        { "compile_commands.json", "Package.swift" },
        ".git",
      }))
    end,
    capabilities = {
      workspace = {
        didChangeWatchedFiles = {
          dynamicRegistration = true,
        },
      },
      textDocument = {
        diagnostic = {
          dynamicRegistration = true,
          relatedDocumentSupport = true,
        },
      },
    },
  },
}

for name, config in pairs(language_servers) do
  vim.lsp.config(name, config)
  vim.lsp.enable(name)
end

-- Too much syntax highlighting with this.
vim.lsp.semantic_tokens.enable(false)

vim.opt.completeopt = { "menuone", "noselect" }

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(event)
    local client = assert(vim.lsp.get_client_by_id(event.data.client_id))

    vim.keymap.set("n", "gd", vim.lsp.buf.definition, {
      buffer = event.buf,
      desc = "LSP Go to Definition",
    })

    if client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, event.buf, {
        autotrigger = false,
      })

      vim.keymap.set("i", "<Tab>", function()
        if vim.fn.pumvisible() == 0 then
          vim.lsp.completion.get()
          return
        end

        local selected = vim.fn.complete_info({ "selected" }).selected
        local keys = selected == -1 and "<C-n><C-y>" or "<C-y>"
        vim.api.nvim_feedkeys(vim.keycode(keys), "n", false)
      end, {
        buffer = event.buf,
        desc = "LSP Completion",
      })
    end
  end,
})

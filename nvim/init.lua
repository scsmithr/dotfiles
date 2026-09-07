vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Use terminal profile colors.
vim.opt.termguicolors = false
vim.cmd.colorscheme("lunaperche")

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

-- System clipboard
vim.opt.clipboard = "unnamedplus"

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
    options = { "--prompt", "Files> ", "--scheme", "path" },
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
vim.keymap.set("n", "<leader>g", fuzzy_grep, { desc = "Fuzzy Grep Project" })

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
}

for name, config in pairs(language_servers) do
  vim.lsp.config(name, config)
  vim.lsp.enable(name)
end

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

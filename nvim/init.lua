vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Use terminal profile colors.
vim.opt.termguicolors = false
vim.cmd.colorscheme("lunaperche")

vim.opt.number = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.scrolloff = 2

vim.opt.wrap = false

-- System clipboard
vim.opt.clipboard = "unnamedplus"

local function fuzzy_find()
  vim.fn["fzf#run"]({
    source = "rg --files",
    sink = function(path)
      vim.cmd.edit(vim.fn.fnameescape(path))
    end,
    options = { "--prompt", "Files> ", "--scheme", "path" },
    window = { width = 0.9, height = 0.6 },
  })
end

-- Bind it to Ctrl + P
vim.keymap.set("n", "<C-p>", fuzzy_find, { desc = "Fuzzy Find Files" })

vim.keymap.set("n", "<leader>gg", require("magit_light").open, { desc = "Git status" })

" Neovim configuration

" Tabs
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set autoindent

" Show line numbers
set number

" Mouse
set mouse=a

" Search
set hlsearch
set incsearch
set showmatch

" Cursor
set guicursor= " Always use block cursor

" Buffers
set hidden " switch between buffers without saving

" Status line
set laststatus=2

" Syntax
set termguicolors
colorscheme base16-two

" Shortcuts

" Leader
let mapleader = ","

" Cycle between buffers with leader-a and leader-s
map <leader>a :bp<CR>
map <leader>s :bn<CR>

" Clear search highlight with leader-c
map <leader>c :nohlsearch<CR>

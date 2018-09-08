" Neovim configuration

" Plugins (vim-plug)
call plug#begin('~/.local/share/nvim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'fatih/vim-go'
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim'

call plug#end()

" Tabs
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set autoindent

" Show line numbers
set number

" Yank to system keyboard
set clipboard=unnamedplus

" Always keep 3 lines visible
set scrolloff=3

" Mouse
set mouse=a

" Search
set hlsearch
set incsearch
set showmatch

" Buffers
set hidden " switch between buffers without saving

" Full height vertical separator
set fillchars+=vert:│

" Status line
set laststatus=2

set statusline=
set statusline +=\[%n]  " buffer number
set statusline +=\ %f     " file name
set statusline +=\ %m   " modified
set statusline +=\ %r   " readonly
set statusline +=\ %*
set statusline +=%=     " left/ right separator
set statusline +=%c     " cursor column
set statusline +=\ :
set statusline +=\ %l/%L%* " current line, total

" Syntax
" set termguicolors
colorscheme base16-two

" Plugin settings

" NERDTree settings
" Expand dir, open files with single click
let NERDTreeMouseMode = 3
let NERDTreeShowHidden = 1
let NERDTreeMinimalUI = 1

" vim-go settings
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1

" Shortcuts

" Leader
let mapleader = ","

" Cycle between buffers with leader-a and leader-s
map <leader>a :bp<CR>
map <leader>s :bn<CR>

" Clear search highlight with leader-c
map <leader>c :nohlsearch<CR>

" Toggle nerdtree with leader-n
map <leader>n :NERDTreeToggle<CR>

" Move to windows with Ctrl-key
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l


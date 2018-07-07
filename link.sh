#!/usr/bin/env bash
# Symlink files to where they're expected to be found

DIR="$(pwd)"

# Tmux
ln -sv $DIR/tmux/tmux.conf ~/.tmux.conf

# vim
ln -sv $DIR/vim/vimrc ~/.vimrc
mkdir -p ~/.vim/colors
ln -sv $DIR/vim/custom-base16-dark.vim ~/.vim/colors
mkdir -p ~/.vim/syntax
ln -sv $DIR/vim/syntax/haskell.vim ~/.vim/syntax
mkdir -p ~/.vim/tmp # Location for swap files

# neovim
mkdir -p ~/.config/nvim
mkdir -p ~/.config/nvim/colors
ln -sv $DIR/nvim/init.vim ~/.config/nvim
ln -sv $DIR/nvim/base16-two.vim ~/.config/nvim/colors

# zsh
ZSH_CONF_DIR=~/.config/zsh-config
mkdir -p $ZSH_CONF_DIR
ln -sv $DIR/zsh/* $ZSH_CONF_DIR
ln -sv $DIR/zsh/zshrc ~/.zshrc

# xmonad
mkdir -p ~/.xmonad
ln -sv $DIR/xmonad/xmonad.hs ~/.xmonad

# xmobar
ln -sv $DIR/xmobar/xmobarrc ~/.xmobarrc

# vscode
ln -sv $DIR/vscode/settings.json ~/.config/Code/User

# various x stuff
ln -sv $DIR/x/xinitrc ~/.xinitrc
ln -sv $DIR/x/xmodmap ~/.xmodmap
ln -sv $DIR/x/Xresources ~/.Xresources

# dunst
mkdir -p ~/.config/dunst
ln -sv $DIR/dunst/dunstrc ~/.config/dunst

# gtk
mkdir -p ~/.config/gtk-3.0
ln -sv $DIR/gtk/settings.ini ~/.config/gtk-3.0
ln -sv $DIR/gtk/gtkrc-2.0 ~/.gtkrc-2.0

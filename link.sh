#!/usr/bin/env bash
# Symlink files to where they're expected to be found

DIR="$(pwd)"

# Tmux
ln -sv $DIR/tmux/tmux.conf ~/.tmux.conf

# vim
ln -sv $DIR/vim/vimrc ~/.vimrc
mkdir -p ~/.vim/colors
ln -sv $DIR/vim/custom-base16-dark.vim ~/.vim/colors

# zsh
ZSH_CONF_DIR=~/.config/zsh-config
mkdir -p $ZSH_CONF_DIR
ln -sv $DIR/zsh/*.zsh $ZSH_CONF_DIR
ln -sv $DIR/zsh/zshrc ~/.zshrc

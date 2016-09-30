#!/bin/env bash
# Symlink files to where they're expected to be found

DIR="$(pwd)"

# Tmux
ln -sv $DIR/tmux/tmux.conf ~/.tmux.conf

# vim
ln -sv $DIR/vim/vimrc ~/.vimrc
mkdir -p ~/.vim/colors
ln -sv $DIR/vim/custom-base16-dark.vim ~/.vim/colors

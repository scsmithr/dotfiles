#!/usr/bin/env bash
# Symlink files to where they're expected to be found

DIR="$(pwd)"

# Tmux
ln -sv $DIR/tmux/tmux.conf ~/.tmux.conf

# vim
ln -sv $DIR/vim/vimrc ~/.vimrc
mkdir -p ~/.vim/colors
ln -sv $DIR/nvim/base16-two.vim ~/.vim/colors
mkdir -p ~/.vim/syntax
ln -sv $DIR/vim/syntax/haskell.vim ~/.vim/syntax
mkdir -p ~/.vim/tmp # Location for swap files

# neovim
mkdir -p ~/.config/nvim
mkdir -p ~/.config/nvim/colors
ln -sv $DIR/nvim/init.vim ~/.config/nvim
ln -sv $DIR/nvim/base16-two.vim ~/.config/nvim/colors
# Install vim-plug
if [ ! -f ~/.local/share/nvim/site/autoload/plug.vim ]; then
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# bash
ln -sv $DIR/bash/bashrc ~/.bashrc

# xmonad
mkdir -p ~/.xmonad/lib
ln -sv $DIR/xmonad/*.hs ~/.xmonad
ln -sv $DIR/xmonad/lib/*.hs ~/.xmonad/lib

# xmobar
ln -sv $DIR/xmobar/xmobarrc ~/.xmobarrc

# vscode
ln -sv $DIR/vscode/settings.json ~/.config/Code/User
ln -sv $DIR/vscode/keybindings.json ~/.config/Code/User

# bin/scripts
mkdir -p ~/.bin
ln -sv $DIR/bin/* ~/.bin

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

# alacritty
mkdir -p ~/.config/alacritty
ln -sv $DIR/alacritty/alacritty.yml ~/.config/alacritty

# rofi
mkdir -p ~/.config/rofi
ln -sv $DIR/rofi/* ~/.config/rofi

# zathura
ln -sv $DIR/zathura/zathurarc ~/.config/zathura

# readline
ln -sv $DIR/readline/inputrc ~/.inputrc

# fonts
mkdir -p ~/.config/fontconfig
ln -sv $DIR/fontconfig/fonts.conf ~/.config/fontconfig

# git
ln -sv $DIR/git/gitconfig ~/.gitconfig
ln -sv $DIR/git/gitignore ~/.gitignore

# emacs
mkdir -p ~/.emacs.d
ln -sv $DIR/emacs/* ~/.emacs.d
mkdir -p ~/.emacs.d/straight/versions/
ln -sv $DIR/emacs/default.el ~/.emacs.d/straight/versions

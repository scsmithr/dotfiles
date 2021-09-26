#!/usr/bin/env bash
# Symlink emacs files to where they're expected to be found

DIR="$(git rev-parse --show-toplevel)"

mkdir -p ~/.emacs.d
ln -sv "$DIR/emacs/early-init.el" ~/.emacs.d/.
ln -sv "$DIR/emacs/init.el" ~/.emacs.d/.
ln -sv "$DIR/emacs/lisp" ~/.emacs.d/.
ln -sv "$DIR/emacs/org-templates" ~/.emacs.d/.
ln -sv "$DIR/emacs/snippets" ~/.emacs.d/.
mkdir -p ~/.emacs.d/straight/versions/
ln -sv "$DIR/emacs/default.el" ~/.emacs.d/straight/versions

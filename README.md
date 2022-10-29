# Dotfiles

My dotfiles using nix.

## Home-manager

Install home-manager using flakes as described [here](https://nix-community.github.io/home-manager/index.html#sec-flakes-standalone).

Switching to a new home-manager revision:

``` shell
home-manager switch --flake .#sean-darwin
```

## Emacs

Running `link.sh` will symlink emacs files to `~/.emacs.d`.

![Emacs screenshot](./screenshot.png)


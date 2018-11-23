autoload -U compinit promptinit colors
compinit
promptinit
colors

# Version control
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
precmd() {
    vcs_info
}

# History
HISTFILE=$HOME/.history
SAVEHIST=10000
HISTSIZE=10000
setopt histverify
setopt inc_append_history

source "$CONFIG_LOCATION/aliases.zsh"
source "$CONFIG_LOCATION/completions.zsh"
source "$CONFIG_LOCATION/environment.zsh"
source "$CONFIG_LOCATION/functions.zsh"
source "$CONFIG_LOCATION/keybinds.zsh"
source "$CONFIG_LOCATION/prompt.zsh"

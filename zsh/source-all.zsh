# Determine operating system
os="$(uname -s)"
linux_str="Linux"
osx_str="Darwin"

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

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*:*:*:*:hosts' list-colors '=*=30;41'
zstyle ':completion:*:*:*:*:users' list-colors '=*=$color[green]=$color[red]'
zstyle ':completion:*' menu select
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# History
HISTFILE=$HOME/.history
SAVEHIST=10000
HISTSIZE=10000
setopt histverify
setopt inc_append_history

# Aliases
source "$CONFIG_LOCATION/aliases.zsh"

# Environment variables
source "$CONFIG_LOCATION/environment.zsh"

# Functions
source "$CONFIG_LOCATION/functions.zsh"

# Keybinds
source "$CONFIG_LOCATION/keybinds.zsh"

# Prompt
source "$CONFIG_LOCATION/prompt.zsh"

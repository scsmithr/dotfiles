# Determine operating system
os="$(uname -s)"
linux_str="Linux"
osx_str="Darwin"

autoload -U compinit promptinit colors
compinit
promptinit
colors

# Prompt
source "$CONFIG_LOCATION/prompt.zsh"

setopt completealiases

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*:*:*:*:hosts' list-colors '=*=30;41'
zstyle ':completion:*:*:*:*:users' list-colors '=*=$color[green]=$color[red]'
zstyle ':completion:*' menu select
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# History
HISTFILE=$HOME/.history
SAVEHIST=1
HISTSIZE=100
setopt histverify

# Aliases
source "$CONFIG_LOCATION/aliases.zsh"

# Environment variables
source "$CONFIG_LOCATION/environment.zsh"

# Functions
source "$CONFIG_LOCATION/functions.zsh"


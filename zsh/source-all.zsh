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

# Prompt
source "$CONFIG_LOCATION/prompt.zsh"

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors 'reply=( "=(#b)(*$VAR)(?)*=00=$color[green]=$color[bg-green]" )'
zstyle ':completion:*:*:*:*:hosts' list-colors '=*=30;41'
zstyle ':completion:*:*:*:*:users' list-colors '=*=$color[green]=$color[red]'
zstyle ':completion:*' menu select
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# Control key fix
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[Od" backward-word
bindkey "^[Oc" forward-word

# History
HISTFILE=$HOME/.history
SAVEHIST=1000
HISTSIZE=1000
setopt histverify
setopt inc_append_history

# Aliases
source "$CONFIG_LOCATION/aliases.zsh"

# Environment variables
source "$CONFIG_LOCATION/environment.zsh"

# Functions
source "$CONFIG_LOCATION/functions.zsh"

# Language specific functions
source "$CONFIG_LOCATION/langs/go.zsh"


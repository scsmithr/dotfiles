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

eval $(dircolors)

zle_highlight=(region:bg=8 special:standout isearch:underline,fg=yellow)

# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ''
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions.
zstyle ':completion:*' completer _complete _list _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Directories
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' special-dirs true

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

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

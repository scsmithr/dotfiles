# shellcheck disable=SC2148

eval $(dircolors)

cdpath=(
    $HOME/.go/src/go.coder.com/
    $HOME/Code/github.com/
)

zle_highlight=(region:bg=0 special:standout isearch:underline,fg=yellow)

# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}%B-- %d (errors: %e) --%b%f'
zstyle ':completion:*:descriptions' format ' %F{cyan}%B-- %d --%b%f'
zstyle ':completion:*:messages' format ' %F{cyan}%B-- %d --%b%f'
zstyle ':completion:*:warnings' format ' %F{red}%B-- no matches --%b%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{cyan}%B-- %d --%b%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions.
zstyle ':completion:*' completer _complete _list _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Directories
zstyle ':completion:*:default' list-colors ${(@s.:.)LS_COLORS}:'ma=01;40'
zstyle ':completion:*:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:*:cd:*' tag-order local-directories named-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' special-dirs true

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# shellcheck disable=SC2148

setopt prompt_subst

PROMPT_TRUNC_WIDTH='${COLUMNS}'

# Version control
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
precmd() {
    vcs_info
}

zstyle ':vcs_info:*' check-for-changes false # set true to show unstaged
zstyle ':vcs_info:git*' formats "%{$fg[green]%}(%b%u)%{$reset_color%}"
zstyle ':vcs_info:git*' actionformats "%{$fg[green]%}(%b%u) $fg[red][%a]%{$reset_color%}"
zstyle ':vcs_info:*' unstagedstr "*"

# Display current vcs status. Single quotes to delay eval
PROMPT_VCS='${vcs_info_msg_0_}'

# Display username@host during ssh sessions.
PROMPT_HOST=""
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    PROMPT_HOST="%{$fg[cyan]%}%n@%m%{$reset_color%} "
fi

# Display current dir, center truncating if path is long.
PROMPT_DIR="%{$fg[blue]%}%(5~|%-1~/.../%3~|%4~)%{$reset_color%}"

# Display vim mode on keymap change.
function zle-line-init zle-keymap-select {
    if [ "$KEYMAP" = vicmd ]; then
        PROMPT_VIM_MODE="%{$fg[yellow]%}N %{$reset_color%}"
    else
        PROMPT_VIM_MODE="%{$fg[green]%}I %{$reset_color%}"
    fi
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

PROMPT="%(?..%F{3}%?%f)%{$reset_color%}
%${PROMPT_TRUNC_WIDTH}>...>$PROMPT_HOST$PROMPT_DIR %(1j.$fg[yellow](%j) $reset_color.)$PROMPT_VCS%>>
\$PROMPT_VIM_MODE%{%(?.$fg[blue].$fg[red])%}>%{$reset_color%} "

RPROMPT=""

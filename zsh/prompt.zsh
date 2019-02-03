# shellcheck disable=SC2148

setopt prompt_subst

PROMPT_TRUNC_WIDTH='${COLUMNS}'
setopt TRANSIENT_RPROMPT # Right prompt removed after enter

# Version control
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
precmd() {
    vcs_info
}

zstyle ':vcs_info:*' check-for-changes false # set true to show unstaged
zstyle ':vcs_info:git*' formats "%{$fg_bold[green]%}(%b%u)%{$reset_color%}"
zstyle ':vcs_info:git*' actionformats "%{$fg_bold[green]%}(%b%u) $fg_bold[red][%a]%{$reset_color%}"
zstyle ':vcs_info:*' unstagedstr "*"

# Display current vcs status. Single quotes to delay eval
PROMPT_VCS='${vcs_info_msg_0_}'

# Display username@host during ssh sessions.
PROMPT_HOST=""
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    PROMPT_HOST="%{$fg_bold[cyan]%}%n@%m%{$reset_color%} "
fi

# Display current dir, center truncating if path is long.
PROMPT_DIR="%{$fg_bold[blue]%}%(5~|%-1~/.../%3~|%4~)%{$reset_color%}"

# Display vim mode on keymap change.
function zle-line-init zle-keymap-select {
    PROMPT_VIM_MODE="%{$fg_bold[yellow]%} -- N --%{$reset_color%}"
    if [ "$KEYMAP" = vicmd ]; then
        echo -ne "\e[1 q"
        RPS1=$PROMPT_VIM_MODE
    else
        echo -ne "\e[5 q"
        RPS1=""
    fi
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

PROMPT="
%${PROMPT_TRUNC_WIDTH}>...>$PROMPT_HOST$PROMPT_DIR %(1j.$fg_bold[yellow](%j) $reset_color.)$PROMPT_VCS%>>
%{%(?.$fg_bold[magenta].$fg_bold[yellow])%}>%{$reset_color%} "

RPROMPT=""

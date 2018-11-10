setopt prompt_subst

zstyle ':vcs_info:*' check-for-changes false # set true to show unstaged
zstyle ':vcs_info:git*' formats "%{$fg[green]%}(%b%u)%{$reset_color%}"
zstyle ':vcs_info:git*' actionformats "%{$fg[red]%}(%b%u)[%a]%{$reset_color%}"
zstyle ':vcs_info:*' unstagedstr "*"

MACHINE=""
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    MACHINE="%{$fg[cyan]%}%n@%m%{$reset_color%} "
fi

CURR_DIR="%{$fg[blue]%}%(5~|%-1~/.../%3~|%4~)%{$reset_color%}"
# Single quotes to delay eval
VCS='${vcs_info_msg_0_}'

TRUNC_WIDTH='${COLUMNS}'
ZLE_RPROMPT_INDENT=0 # No extra space after right prompt
setopt TRANSIENT_RPROMPT # Right prompt removed after enter

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} -- NORMAL --%{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

PROMPT="
%${TRUNC_WIDTH}>...>$MACHINE$CURR_DIR %(1j.$fg[yellow](%j) $reset_color.)$VCS%>>
%{%(?.$fg[magenta].$fg[yellow])%}>%{$reset_color%} "

RPROMPT=""

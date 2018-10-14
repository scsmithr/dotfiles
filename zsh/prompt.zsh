setopt prompt_subst

zstyle ':vcs_info:*' check-for-changes false # set true to show unstaged
zstyle ':vcs_info:git*' formats "%{$fg[green]%}(%b%u)%{$reset_color%}"
zstyle ':vcs_info:git*' actionformats "%{$fg[red]%}(%b%u)[%a]%{$reset_color%}"
zstyle ':vcs_info:*' unstagedstr "*"

MACHINE=""
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    MACHINE="%{$fg[magenta]%}%n@%m%{$reset_color%}:"
fi

CURR_DIR="%{$fg[blue]%}%(5~|%-1~/.../%3~|%4~)%{$reset_color%}"
# Single quotes to delay eval
VCS='${vcs_info_msg_0_}'

TRUNC_WIDTH='${COLUMNS}'

PROMPT="
%${TRUNC_WIDTH}>...>$MACHINE$CURR_DIR %(1j.$fg[yellow](%j) $reset_color.)$VCS%>>
%{%(?.$fg[magenta].$fg[yellow])%}>%{$reset_color%} "

RPROMPT=""

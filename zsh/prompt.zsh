setopt prompt_subst

zstyle ':vcs_info:*' check-for-changes false # set true to show unstaged
zstyle ':vcs_info:git*' formats "%{$fg[green]%}(%b%u)%{$reset_color%}"
zstyle ':vcs_info:*' unstagedstr "*"

MACHINE=""
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    MACHINE="%{$fg[cyan]%}%n@%m%{$reset_color%}:"
fi

CURR_DIR="%{$fg[blue]%}%~%{$reset_color%}"
# Single quotes to delay eval
VCS='${vcs_info_msg_0_}'

PROMPT="
$MACHINE$CURR_DIR $VCS
%{$fg[magenta]%}>%{$reset_color%} "

RPROMPT=""

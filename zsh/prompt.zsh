MACHINE=""
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    MACHINE="%{$fg[cyan]%}%n@%m%{$reset_color%}:"
fi

CURR_DIR="%{$fg[blue]%}%~%{$reset_color%}"

PROMPT="
$MACHINE$CURR_DIR
%{$fg[magenta]%}>%{$reset_color%} "


# shellcheck disable=SC2148

alias ls='ls --color=auto'
alias dir='dir --color=auto'

alias no="notify normal"
alias noc="notify critical"
alias nol="notify low"

# Some useful tmux alias
alias t='tmux new-session -s' # Create new session, should pass name
alias ta='tmux a -t' # Attach to a session
alias tn='tmux new-session -t' # Attach to a session, independent control
alias tl='tmux ls'
alias tk='tmux kill-session -t'

alias nv=nvim

alias rc="findedit -e 'code -g {f}:{l}' -l"
alias rv="findedit -e 'nvim +{l} {f}' -l"

alias grep='grep --color=always'

# trash-cli
alias tm=trash-put

alias mm='xmodmap ~/.xmodmap'

# Always keep a log when using mycli.
alias mycli="mycli -l \$(ensure_log mycli)"

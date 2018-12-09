alias ls='ls --color=auto'
alias dir='dir --color=auto'

# Pacman aliases
alias pacman=run_pacman

# ssh aliases
alias usshfs='fusermount -u $HOME/Remote'

# set monitor brighness
alias sb=set_brightness
alias sbr=set_brightness_rel
# get current brightness
alias gb=get_brightness

alias no="notify normal"
alias noc="notify critical"
alias nol="notify low"

alias ll='exa -lG'
alias la='exa -alG'

alias gcssh='TERM=xterm-256color gcloud compute ssh'

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

# Compression aliases
alias tarx='tar -xvf'
alias targz='tar -zxvf'
alias tarbz2= 'tar -jxvf'

# Aliases for stack managed executables
alias sghc='stack ghc'
alias srunghc='stack runghc'

# shellcheck disable=SC2148

export PATH=$PATH:$HOME/.bin/:$HOME/.local/bin/

# Set default go workspace
export GOPATH=$HOME/.go
export PATH=$PATH:$HOME/.go/bin
export PATH=$PATH:/usr/local/go/bin

# Cargo bins
export PATH=$PATH:$HOME/.cargo/bin

# Cabal bins
export PATH=$PATH:$HOME/.cabal/bin

export XKB_DEFAULT_OPTIONS=ctrl:nocaps

# Default fzf options
export FZF_DEFAULT_OPTS='
    --color=16
    --color hl:5,hl+:5,fg+:4,prompt:7,pointer:5,info:5
    --height=10
    --min-height=5
    --layout=reverse
'

# Make prompt toolkit programs (mycli) use ansi colors only.
export PROMPT_TOOLKIT_COLOR_DEPTH='DEPTH_4_BIT'

export LOCAL_LOGS_DIR="$HOME/.logs"

# man colors
# https://unix.stackexchange.com/questions/119/colors-in-man-pages
# https://unix.stackexchange.com/questions/108699/documentation-on-less-termcap-variables
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2)
export LESS_TERMCAP_md=$(tput bold; tput setaf 4)
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3)
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul)
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)

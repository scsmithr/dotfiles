export PATH=$PATH:$HOME/.bin/

# Set default go workspace
export GOPATH=$HOME/.go
export PATH=$PATH:$HOME/.go/bin
export PATH=$PATH:/usr/local/go/bin

# Cargo bins
export PATH=$PATH:$HOME/.cargo/bin

# Default fzf options
export FZF_DEFAULT_OPTS='
    --color=16
    --height=10
    --min-height=5
    --layout=reverse
'

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

if [[ "$os" = "$linux_str" ]]; then
    #PATH
    export PATH=$PATH:$HOME/.cabal/bin
    export PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin

    export XKB_DEFAULT_OPTIONS=ctrl:nocaps
    
elif [[ "$os" = "$osx_str" ]]; then
    #PATH
    export PATH=$PATH:$HOME/Library/Haskell/bin
    export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.4/bin
    
    # Kubernetes
    export PATH=$PATH:$HOME/kubernetes/platforms/darwin/amd64
fi

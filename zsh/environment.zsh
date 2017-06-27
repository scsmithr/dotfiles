export PATH=$PATH:$HOME/.bin/

# Set default go workspace
export GOPATH=$HOME/.go
export PATH=$PATH:$HOME/.go/bin

# Cargo bins
export PATH=$PATH:$HOME/.cargo/bin

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

    # Go
    export PATH=$PATH:/usr/local/go/bin
fi

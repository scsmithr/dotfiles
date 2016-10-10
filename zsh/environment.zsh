export PATH=$PATH:$HOME/.bin/

# Set default go workspace
export GOPATH=$HOME/.go
export PATH=$PATH:$HOME/.go/bin

if [[ "$os" = "$linux_str" ]]; then
    #PATH
    export PATH=$PATH:$HOME/.cabal/bin
    export PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
    
elif [[ "$os" = "$osx_str" ]]; then
    #PATH
    export PATH=$PATH:$HOME/Library/Haskell/bin
    export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.4/bin
    # Go
    export PATH=$PATH:/usr/local/go/bin

    # Docker
    export DOCKER_HOST=tcp://192.168.59.103:2376
    export DOCKER_CERT_PATH=$HOME/.boot2docker/certs/boot2docker-vm
    export DOCKER_TLS_VERIFY=1

fi

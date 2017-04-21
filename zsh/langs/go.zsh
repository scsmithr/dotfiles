gopath() {
    if [[ "$#" -eq 0 ]]; then
        print "No arguments"
    else
        local abs_path=`cd "$1"; pwd`
        GOPATH=$GOPATH:$abs_path
        PATH=$PATH:$abs_path/bin
    fi
}

# shellcheck disable=SC2148

# Run pacman, prompting for password if needed
run_pacman() {
    case $1 in
        -S | -D | -S[^sih]* | -R* | -U*)
            /usr/bin/sudo /usr/bin/pacman "$@" ;;
    *)      /usr/bin/pacman "$@" ;;
    esac
}

dpi() {
    if [ "$#" -eq 1 ]; then
        echo "Xft.dpi: $1" | xrdb -merge
        echo "*dpi: $1" | xrdb -merge
    else 
        print "Invalid number of arguments"
    fi
}

jd() {
    local dir
    dir=$(find ${1:-~} -maxdepth 7 -type d 2> /dev/null | fzf +m)
    if [ ! -z "$dir" ]; then
        cd "$dir"
    fi
}

refresh() {
    source "$CONFIG_LOCATION/init.zsh"
}

# Create a directory and cd into it
mkcd() {
    if [[ "$#" -eq 0 ]]; then
        print "No arguments"
    else
        if [[ "$#" -gt 1 ]]; then
            print "Only creating $1"
        fi
        mkdir "$1"
        cd "$1"
    fi
}

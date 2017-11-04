if [[ "$os" = "$linux_str" ]]; then
    # Open the specified directory in nautilus or other provided program
    open() {
        if [ "$#" -eq 0 ]; then
            xdg-open . &>/dev/null &
        elif [ "$#" -eq 1 ]; then
            xdg-open "$1" &>/dev/null &
        else
            nohup "$1" "$2" &>/dev/null &
        fi
    }

    # Notify after a process has completed
    notify() {
        $@
        notify-send "Process Completed" "$*"
    }

    # Run pacman, prompting for password if needed
    run_pacman() {
        case $1 in
            -S | -D | -S[^sih]* | -R* | -U*)
                /usr/bin/sudo /usr/bin/pacman "$@" ;;
        *)      /usr/bin/pacman "$@" ;;
        esac
    }

    set_brightness() {
        if [ "$#" -ne 1 ]; then
            print "Invalid number of arguments"
        else
            ddcutil setvcp 10 $1
        fi
    }
fi

# Start ssh daemon
startssh() {
    if [[ "$os" = "$linux_str" ]]; then
        systemctl start sshd
    elif [[ "$os" = "$osx_str" ]]; then
        sudo systemsetup -f -setremotelogin on
    fi
}

# Stop ssh daemon
stopssh() {
    if [[ "$os" = "$linux_str" ]]; then
        systemctl stop sshd
    elif [[ "$os" = "$osx_str" ]]; then
        sudo systemsetup -f -setremotelogin off
    fi
}

run_man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    /usr/bin/man "$@"
}

refresh() {
    source "$CONFIG_LOCATION/source-all.zsh"
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

# move files/ directories to trash
trash() {
    if [[ "$#" -eq 0 ]]; then
        print "No arguments"
    else
        if [ "$os" = "$linux_str" ]; then
            mv "$@" "$HOME/.local/share/Trash/files/."
        elif [ "$os" = "$osx_str" ]; then
            mv "$@" "$HOME/.Trash/."
        fi
    fi
}

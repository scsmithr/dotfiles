# shellcheck disable=SC2148

ec() {
    emacs $@ &!
}

dpi() {
    if [ "$#" -eq 1 ]; then
        echo "Xft.dpi: $1" | xrdb -merge
        echo "*dpi: $1" | xrdb -merge
    else
        print "Invalid number of arguments"
    fi
}

bribribri() {
    set_bri=$1
    bri monitor set $set_bri; bri monitor set $set_bri; bri monitor set $set_bri
}

gcip() {
    if [ "$#" -ge 1 ]; then
        gcloud compute instances describe $@ | grep "natIP" | awk '{print $2}' | xargs echo -n
    else
        print "Invalid number of arguments"
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

ensure_log() {
    if [[ "$#" -eq 0 ]]; then
        print "No arguments"
    else
        local dir
        dir="$LOCAL_LOGS_DIR/$1"
        mkdir -p "$dir"
        local name
        name="$dir/$(date +%F).log"
        touch "$name"
        echo "$name"
    fi
}

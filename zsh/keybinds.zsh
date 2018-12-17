# shellcheck disable=SC2148

export KEYTIMEOUT=1

# Control key fix
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[Od" backward-word
bindkey "^[Oc" forward-word

bindkey -v

bindkey -M viins '^r' history-incremental-search-backward
bindkey -M viins '^p' find_history

bindkey "^?" backward-delete-char

function find_history() {
    local sel
    sel=$( (fc -l -n 1) | rg "$BUFFER" | uniq | fzf +s --tac)
    if [ -n "$sel" ]; then
        BUFFER="$sel"
        zle .reset-prompt
        zle .accept-line
    else
        zle .reset-prompt
    fi
}
zle -N find_history

# Taken from https://github.com/robbyrussell/oh-my-zsh/blob/0f6e49b455e498bd051d1d18d62dec4e6872d3e8/plugins/vi-mode/vi-mode.plugin.zsh
# Allow Copy/Paste with the system clipboard
# behave as expected with vim commands ( y/p/d/c/s )
[[ -n $DISPLAY ]] && (( $+commands[xsel] )) && {

    function cutbuffer() {
        zle .$WIDGET
        echo $CUTBUFFER | xsel -ib
    }

    zle_cut_widgets=(
        vi-backward-delete-char
        vi-change
        vi-change-eol
        vi-change-whole-line
        vi-delete
        vi-delete-char
        vi-kill-eol
        vi-substitute
        vi-yank
        vi-yank-eol
    )

    for widget in $zle_cut_widgets
    do
        zle -N $widget cutbuffer
    done

    function putbuffer() {
        zle copy-region-as-kill "$(xsel -ob)"
        zle .$WIDGET
    }

    zle_put_widgets=(
        vi-put-after
        vi-put-before
    )
    for widget in $zle_put_widgets
    do
        zle -N $widget putbuffer
    done
}

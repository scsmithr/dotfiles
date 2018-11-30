# ZSH autosuggestions
if [ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
    ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
    ZSH_AUTOSUGGEST_USE_ASYNC=1

    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

    bindkey -M viins '^f' autosuggest-accept
fi

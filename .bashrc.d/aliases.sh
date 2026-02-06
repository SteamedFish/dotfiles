#!/usr/bin/env bash

alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
alias ln='ln -iv'
alias rsync='rsync --partial --secluded-args --progress --human-readable --info=PROGRESS2'
if [ -n "$(command -v eza)" ]; then
    if [[ "$OSTYPE" == "linux-android" ]]; then
        # not support --icons
        alias ls='eza --git --extended'
        alias tree='eza --tree --git --extended'
    else
        alias ls='eza --git --icons --color=auto --header --group --mounts'
        alias tree='eza --tree --git --icons --color=auto'
    fi
elif [ -n "$(command -v lsd)" ]; then
    alias ls='lsd'
    alias tree='lsd --tree'
fi
if [ -n "$(command -v fdfind)" ]; then
    # Debian use this name
    alias fd='fdfind'
fi
# Setup bat/batcat (Debian uses 'batcat' name)
_setup_bat_aliases() {
    local bat_cmd="$1"
    alias cat="$bat_cmd"
    export MANPAGER="$bat_cmd --paging=auto -plman --theme 'Monokai Extended'"
    if [ "$(basename "$SHELL")" = "zsh" ]; then
        alias -g -- --help="--help 2>&1 | $bat_cmd --language=help --style=plain"
    fi
}

if [ -n "$(command -v bat)" ]; then
    _setup_bat_aliases "bat"
elif [ -n "$(command -v batcat)" ]; then
    _setup_bat_aliases "batcat"
fi
if [ -n "$(command -v lesspipe.sh)" ]; then
    # lesspipe already set in /etc/profile.d/lesspipe.sh
    :
elif [ -n "$(command -v batpipe)" ]; then
    eval "$(batpipe)"
fi
if [ -n "$(command -v nvim)" ]; then
    alias vim='nvim'
    alias vi='nvim'
    alias vimdiff='nvim -d'
fi
if [ -n "$(command -v dool)" ]; then
    alias dstat='dool'
elif [ -n "$(command -v dstat)" ]; then
    alias dool='dstat'
fi

if [ -n "$(command -v direnv)" ]; then
    if [ "$(basename "$SHELL")" = "zsh" ]; then
        eval "$(direnv hook zsh)"
    elif [ -n "$BASH" ]; then
        eval "$(direnv hook bash)"
    fi
fi

if [ -n "$(command -v nerdctl)" ]; then
    alias docker="nerdctl"
fi

if [ -n "$(command -v zoxide)" ]; then
    if [ "$(basename "$SHELL")" = "zsh" ]; then
        eval "$(zoxide init --cmd cd zsh)"
    elif [ -n "$BASH" ]; then
        eval "$(zoxide init --cmd cd bash)"
    fi
fi

if [ -n "$(command -v atuin)" ]; then
    if [ "$(basename "$SHELL")" = "zsh" ]; then
        eval "$(atuin init zsh --disable-up-arrow)"
    elif [ -n "$BASH" ]; then
        eval "$(atuin init bash --disable-up-arrow)"
    fi
fi

#if [ -n "$(command -v easytier-cli)" ]; then
#    if [ "$(basename "$SHELL")" = "zsh" ]; then
#        eval "$(easytier-cli gen-autocomplete zsh)"
#    elif [ -n "$BASH" ]; then
#        eval "$(easytier-cli gen-autocomplete bash)"
#    fi
#fi

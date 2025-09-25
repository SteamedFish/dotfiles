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
if [ -n "$(command -v bat)" ]; then
    alias cat='bat'
    if [ -n "$(command -v batman)" ]; then
        eval "$(batman --export-env)"
    elif [ -n "$(command -v awk)" ]; then
        export MANPAGER="sh -c 'awk '\''{ gsub(/\x1B\[[0-9;]*m/, \"\", \$0); gsub(/.\x08/, \"\", \$0); print }'\'' | bat -p --paging=always -lman'"
    elif [ -n "$(command -v col)" ]; then
        export MANPAGER="sh -c 'col -bx | bat -l man --paging=always -p'"
    fi
    if [ "$(basename "$SHELL")" = "zsh" ]; then
        #alias -g -- -h='-h 2>&1 | bat --language=help --style=plain'
        alias -g -- --help='--help 2>&1 | bat --language=help --style=plain'
    fi
fi
if [ -n "$(command -v batcat)" ]; then
    alias cat='batcat'
    if [ -n "$(command -v batman)" ]; then
        eval "$(batman --export-env)"
    elif [ -n "$(command -v awk)" ]; then
        export MANPAGER="sh -c 'awk '\''{ gsub(/\x1B\[[0-9;]*m/, \"\", \$0); gsub(/.\x08/, \"\", \$0); print }'\'' | batcat -p --paging=always -lman'"
    elif [ -n "$(command -v col)" ]; then
        export MANPAGER="sh -c 'col -bx | batcat -l man --paging=always -p'"
    fi
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

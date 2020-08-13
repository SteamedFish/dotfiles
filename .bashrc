#!/bin/bash

# do nothing if not running interactively

case $- in
    *i*) ;;
    *) return;;
esac

# exports
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LESS="-R"
export CVS_RSH=ssh
export EDITOR="vim"
export MYSQL_PS1="(\u@\h) [\d]> "
export PATH=$PATH:$HOME/.go/bin:$HOME/.cargo/bin:$HOME/.local/bin:~/bin
export GOPATH=$HOME/.go


# enable color support
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    export CLICOLOR=true
fi

if $CLICOLOR ;then
    if [ -x /usr/bin/dircolors ]; then
        if [ -r ~/.dircolors ]; then
            eval "$(dircolors -b ~/.dircolors)"
        else
            eval "$(dircolors -b)"
        fi
    elif [ -x /usr/local/bin/gdircolors ]; then
        if [ -r ~/.dircolors ]; then
            eval "$(gdircolors -b ~/.dircolors)"
        else
            eval "$(gdircolors -b)"
        fi
    fi
fi

# Aliases
if $CLICOLOR; then
    alias grep='grep --color=auto'
    alias egrep='egrep --colour=auto'
    alias fgrep='fgrep --colour=auto'
    alias ip='ip --color=auto'
    alias diff='diff --color=auto'
    if [ -n "$(command -v grc)" ]; then
        if [ -n "$BASH" ]; then
            if [ -f "/etc/profile.d/grc.bashrc" ]; then
                source "/etc/profile.d/grc.bashrc"
            elif [ -f "/usr/share/grc/grc.bashrc" ]; then
                source "/usr/share/grc/grc.bashrc"
            fi
        elif [ "$(basename "$SHELL")" = "zsh" ]; then
            if [ -f "/etc/grc.zsh" ]; then
                source "/etc/grc.zsh"
            elif [ -f "/usr/share/grc/grc.zsh" ]; then
                source "/usr/share/grc/grc.zsh"
            fi
        fi
    fi
fi
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
if [ -n "$(command -v lsd)" ];then
    alias ls='lsd'
    alias tree='lsd --tree'
elif [ -n "$(command -v exa)" ];then
    alias ls='exa --git --icons --extended'
    alias tree='exa --tree --git --icons --extended'
fi
if [ -n "$(command -v fdfind)" ];then
    # Debian use this name
    alias fd='fdfind'
fi
if [ -n "$(command -v bat)" ];then
    alias cat='bat'
fi
if [ -n "$(command -v nvim)" ];then
    alias vim='nvim'
    alias vi='nvim'
fi

if [ -n "$(command -v direnv)" ]; then
    if [ -n "$BASH" ]; then
        eval "$(direnv hook bash)"
    elif [ "$(basename "$SHELL")" = "zsh" ]; then
        eval "$(direnv hook zsh)"
    fi
fi

if [ -e "$HOME/.bashrc.d" ]; then
    for i in "$HOME/.bashrc.d/"*.sh; do
        if [ -r "$i" ]; then
            # shellcheck disable=SC1090
            source "$i"
        fi
    done
    unset i
fi

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
export PATH=$PATH:~/bin
export GOPATH=$HOME/.go


# enable color support
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    export CLICOLOR=true
fi

if $CLICOLOR ;then
    if [ -x /usr/bin/dircolors ]; then
        test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    elif [ -x /usr/local/bin/gdircolors ]; then
        test -r ~/.dircolors && eval "$(gdircolors -b ~/.dircolors)" || eval "$(gdircolors -b)"
    fi
fi

# Aliases
if $CLICOLOR; then
    alias grep='grep --color=auto'
    alias egrep='egrep --colour=auto'
    alias fgrep='fgrep --colour=auto'
fi
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
if [ -n "$(command -v lsd)" ];then
    alias ls='lsd'
    alias tree='lsd --tree'
elif [ -n "$(command -v exa)" ];then
    alias ls='exa --git --extended'
    alias tree='exa --tree --git --extended'
fi
if [ -n "$(command -v nvim)" ];then
    alias vim='nvim'
    alias vi='nvim'
fi

if [ -e "$HOME/.bashrc.d" ]; then
    for i in $HOME/.bashrc.d/*.sh; do
        if [ -r $i ]; then
            source $i
        fi
    done
    unset i
fi

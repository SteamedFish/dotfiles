#!/bin/bash

# exports
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LESS="-R"
export CVS_RSH=ssh
export EDITOR="vim"
export MYSQL_PS1="(\u@\h) [\d]> "
export PATH=$PATH:$HOME/.go/bin:$HOME/.cargo/bin:$HOME/.local/bin
export GOPATH=$HOME/.go

# do nothing if not running interactively
case $- in
*i*) ;;
*) return ;;
esac

# enable color support
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    export CLICOLOR=true
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

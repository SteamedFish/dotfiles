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
    # mac's `uname' don't have -o option
    if [[ "$(uname -o 2>&1 >/dev/null)" != "Android" ]]; then
        alias ip='ip --color=auto'
        alias diff='diff --color=auto'
    fi
    if [ -n "$(command -v grc)" ]; then
        for _dir in "/etc" "/etc/profile.d" "/usr/share/grc" "/usr/local/etc"; do
            if [ -n "$BASH" ]; then
                if [ -f "${_dir}/grc.bashrc" ]; then
                    # shellcheck disable=SC1090
                    source "${_dir}/grc.bashrc"
                    break
                fi
            elif [ "$(basename "$SHELL")" = "zsh" ]; then
                if [ -f "${_dir}/grc.zsh" ]; then
                    # shellcheck disable=SC1090
                    source "${_dir}/grc.zsh"
                    break
                fi
            fi
        done
        unset _dir
    fi
fi
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
if [ -n "$(command -v lsd)" ];then
    alias ls='lsd'
    alias tree='lsd --tree'
elif [ -n "$(command -v exa)" ];then
    if [[ "$(uname -o 2>&1 >/dev/null)" == "Android" ]]; then
        alias ls='exa --git --extended'
        alias tree='exa --tree --git --extended'
    else
        alias ls='exa --git --icons --extended'
        alias tree='exa --tree --git --icons --extended'
    fi
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

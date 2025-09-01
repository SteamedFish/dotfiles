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

if $CLICOLOR; then
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
    if [[ "$OSTYPE" != "linux-android" ]] && [[ "$OSTYPE" != "darwin"* ]]; then
        alias ip='ip --color=auto'
        alias diff='diff --color=auto'
    elif [[ "$OSTYPE" == "linux-android" ]]; then
        alias ip='ip -color'
    fi
    if [ -n "$(command -v grc)" ]; then
        for _dir in "/etc" "/etc/profile.d" "/usr/share/grc" "/usr/local/etc"; do
            if [ -n "$BASH" ]; then
                if [ -f "${_dir}/grc.bashrc" ]; then
                    # shellcheck disable=SC1091
                    source "${_dir}/grc.bashrc"
                    break
                fi
            elif [ "$(basename "$SHELL")" = "zsh" ]; then
                if [ -f "${_dir}/grc.zsh" ]; then
                    # shellcheck disable=SC1091
                    source "${_dir}/grc.zsh"
                    break
                fi
            fi
        done
        unset _dir
    fi
    export LESS="-R -s -M -F"
    export LESS_TERMCAP_mb=$'\E[1;31m'     # begin blink
    export LESS_TERMCAP_md=$'\E[1;36m'     # begin bold
    export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
    export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
    export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
    export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
    export LESS_TERMCAP_ue=$'\E[0m'        # reset underline
    export GROFF_NO_SGR=1                  # For Konsole and Gnome-terminal
fi
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
        export MANPAGER="sh -c 'awk '\''{ gsub(/\x1B\[[0-9;]*m/, \"\", \$0); gsub(/.\x08/, \"\", \$0); print }'\'' | bat -p -lman'"
    elif [ -n "$(command -v col)" ]; then
        export MANPAGER="sh -c 'col -bx | bat -l man --paging=always -p'"
    fi
    if [ "$(basename "$SHELL")" = "zsh" ]; then
        alias -g -- --help='--help 2>&1 | bat --language=help --style=plain'
    fi
fi
if [ -n "$(command -v batcat)" ]; then
    alias cat='batcat'
    if [ -n "$(command -v batman)" ]; then
        eval "$(batman --export-env)"
    elif [ -n "$(command -v awk)" ]; then
        export MANPAGER="sh -c 'awk '\''{ gsub(/\x1B\[[0-9;]*m/, \"\", \$0); gsub(/.\x08/, \"\", \$0); print }'\'' | batcat -p -lman'"
    elif [ -n "$(command -v col)" ]; then
        export MANPAGER="sh -c 'col -bx | batcat -l man --paging=always -p'"
    fi
fi
if [ -n "$(command -v nvim)" ]; then
    alias vim='nvim'
    alias vi='nvim'
fi
if [ -n "$(command -v dool)" ]; then
    alias dstat='dool'
fi

if [ -n "$(command -v direnv)" ]; then
    if [ -n "$BASH" ]; then
        eval "$(direnv hook bash)"
    elif [ "$(basename "$SHELL")" = "zsh" ]; then
        eval "$(direnv hook zsh)"
    fi
fi

if [ -n "$(command -v nerdctl)" ]; then
    alias docker="nerdctl"
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

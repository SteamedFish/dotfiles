#!/usr/bin/env bash

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
            if [ "$(basename "$SHELL")" = "zsh" ]; then
                if [ -f "${_dir}/grc.zsh" ]; then
                    # shellcheck disable=SC1091
                    source "${_dir}/grc.zsh"
                    break
                fi
            elif [ -n "$BASH" ]; then
                if [ -f "${_dir}/grc.bashrc" ]; then
                    # shellcheck disable=SC1091
                    source "${_dir}/grc.bashrc"
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

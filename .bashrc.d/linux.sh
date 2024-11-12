#!/usr/bin/env bash

# Linux related
if [ "$OSTYPE" != "linux-gnu" ]; then
    return
fi

if $CLICOLOR; then
    if ! alias ls >/dev/null 2>&1; then
        alias ls='ls --color=auto'
    fi
fi
alias open='xdg-open'
alias pbcopy='xclip -selection c -i'
alias pbpaste='xclip -selection c -o'

if [ -f "$HOME"/.ssh/id_rsa ]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
fi

GPG_TTY=$(tty)
export GPG_TTY

# by default Linux don't show pings that don't get replied
# we want to show that
if [ -n "$(command -v grc)" ]; then
    alias ping="grc --colour=auto ping -O"
else
    alias ping="ping -O"
fi

export MAKEFLAGS="-j$(nproc)"

# extra PATHs
for i in /home/linuxbrew/.linuxbrew $HOME/.linuxbrew /snap /opt/puppetlabs/bin; do
    if [ -d "$i" ]; then
        export PATH="$i/bin:$i/sbin/$PATH"
    fi
done
unset i

if [ -n "$BASH" ]; then
    # Change the window title of X terminals
    case ${TERM} in
    xterm* | rxvt* | Eterm* | aterm | kterm | gnome* | interix | konsole*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
    screen*)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
        ;;
    esac
fi

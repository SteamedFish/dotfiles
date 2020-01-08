#!/usr/bin/env bash

# Linux related
if [ "$OSTYPE" != "linux-gnu" ]; then
    return
fi

if $CLICOLOR ; then
    if ! alias ls > /dev/null 2>&1; then
        alias ls='ls --color=auto'
    fi
fi
alias open='xdg-open'

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export XIM=fcitx
export XIM_PROGRAM="fcitx"

if [ -f "$HOME"/.ssh/id_rsa ]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
fi

export GPG_TTY=$(tty)

# extra PATHs
for i in /home/linuxbrew/.linuxbrew $HOME/.linuxbrew /snap/bin; do
    if [ -d "$i" ]; then
        export PATH="$i/bin:$i/sbin/$PATH"
    fi
done
unset i

if [ -n "$BASH" ]; then
    # Change the window title of X terminals
    case ${TERM} in
        xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
            PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
            ;;
        screen*)
            PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
            ;;
    esac
fi

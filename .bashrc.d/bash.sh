#!/usr/bin/env bash

if [ -z "$BASH" ]; then
    return
fi

# History
export HISTCONTROL=ignoredups
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT="%F %T"
export HISTTIMEFORMAT="[%Y-%m-%d %H:%M:%S]  "
export HISTFILESIZE=-1
shopt -s histappend

# PS1
if $CLICOLOR ;then
    if [ "$TERM" == "eterm-color" ]; then
        # emacs handles wrong with color reset
        export PS1='\[\033[01;32m\]\u\[\033[01;34m\]@\[\033[01;32m\]\h:\[\033[01;34m\]\w\[\033[00m\]\$ '
    elif [ "$TERM" == "dumb" ]; then
        export PS1="> "
    else
        export PS1='\[\033[01;32m\]\u\[\033[01;34m\]@\[\033[01;32m\]\h\[\033[00m\[:\[\033[01;34m\]\w\[\033[00m\]\$ '
    fi
fi

# bash-completion
if [ -n "$BASH_VERSION" -a -z "$BASH_COMPLETION" ]; then
    if [ -r /usr/local/etc/bash_completion ]; then
        . /usr/local/etc/bash_completion
    fi
    if [ -r /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
    if [ -r /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    fi
    if [ -r $HOME/.bash_completion ]; then
        . $HOME/.bash_completion
    fi
    if [ -n "$(command -v pandoc)" ];then
        eval "$(pandoc --bash-completion)"
    fi
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

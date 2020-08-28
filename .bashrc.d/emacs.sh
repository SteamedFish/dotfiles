#!/usr/bin/env bash

if [ -d "$HOME/.emacs.d/bin" ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

# emacs-vterm
if [ -n "$INSIDE_EMACS" ]; then
    alias vi='emacsclient -n'
    alias vim='emacsclient -n'
    alias emacs="emacsclient -n"
fi

if [[ $(uname -o) == "Android" ]]; then
    return
fi

# https://wiki.archlinux.org/index.php/Emacs#Multiplexing_emacs_and_emacsclient
function emacs {
    if [[ $# -eq 0 ]]; then
        if [[ "$OSTYPE" == "darwin"* ]]; then
            /Applications/Emacs.app/Contents/MacOS/Emacs
        elif [[ "$OSTYPE" == "linux-gnu" ]]; then
            /usr/bin/emacs # "emacs" is function, will cause recursion
        fi
        return
    fi
    args=("$*")
    for ((i=0; i <= ${#args}; i++)); do
        local a=${args[i]}
        # NOTE: -c for creating new frame
        if [[ ${a:0:1} == '-' && ${a} != '-c' && ${a} != '--' ]]; then
            if [[ "$OSTYPE" == "darwin"* ]]; then
                /Applications/Emacs.app/Contents/MacOS/Emacs "${args[*]}"
            elif [[ "$OSTYPE" == "linux-gnu" ]]; then
                /usr/bin/emacs "${args[*]}"
            fi
            return
        fi
    done
    if [[ "$OSTYPE" == "darwin"* ]]; then
        emacsclient -n -a /Applications/Emacs.app/Contents/MacOS/Emacs "${args[*]}"
    elif [[ "$OSTYPE" == "linux-gnu" ]]; then
        setsid emacsclient -n -a /usr/bin/emacs "${args[*]}"
    fi
}

if [ -d "$HOME/emacs-distros/spacemacs" ] && \
    [ -f "$HOME/.emacs-profiles.el" ]; then
    for distro in centaur spacemacs; do
        eval "
        function $distro {
            ln -s emacs-distros/chemacs/.emacs ~/
            if [[ \"$OSTYPE\" == \"darwin\"* ]]; then
                /Applications/Emacs.app/Contents/MacOS/Emacs --with-profile=$distro &
            elif [[ \"$OSTYPE\" == \"linux-gnu\" ]]; then
                /usr/bin/emacs --with-profile=$distro &
            fi
            sleep 10 && rm -f ~/.emacs &
        }
        "
    done
    unset distro
fi

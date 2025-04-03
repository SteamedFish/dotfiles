#!/usr/bin/env bash

if [ -d "$HOME/.config/emacs/bin" ]; then
    export PATH="$HOME/.config/emacs/bin:$PATH"
elif [ -d "$HOME/.emacs.d/bin" ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [[ "$OSTYPE" == "linux-android" ]]; then
    return
fi

if [ -x "$HOME/.local/bin/Emacs.AppImage" ]; then
    alias emacs="$HOME/.local/bin/Emacs.AppImage"
    alias emacsclient="$HOME/.local/bin/Emacs.AppImage --emacs-appimage-run-as emacsclient"
fi

if [ "$INSIDE_EMACS" = "vterm" ]; then
    alias vi='emacsclient -n'
    alias vim='emacsclient -n'
    alias emacs="emacsclient -n"

    # https://github.com/akermu/emacs-libvterm#shell-side-configuration
    vterm_printf() {
        if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
            # Tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }

    vterm_prompt_end() {
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }

    if [ "$(basename "$SHELL")" = "zsh" ]; then
        setopt PROMPT_SUBST
        PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
    elif [ -n "$BASH" ]; then
        PS1=$PS1'\[$(vterm_prompt_end)\]'
    fi
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
    for ((i = 0; i <= ${#args}; i++)); do
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

if [ -d "$HOME/emacs-distros/spacemacs" ] &&
    [ -f "$HOME/.emacs-profiles.el" ]; then
    for distro in centaur spacemacs; do
        eval "
        function $distro {
            if [[ \"$OSTYPE\" == \"darwin\"* ]]; then
                /Applications/Emacs.app/Contents/MacOS/Emacs --with-profile=$distro &
            elif [[ \"$OSTYPE\" == \"linux-gnu\" ]]; then
                /usr/bin/emacs --with-profile=$distro &
            fi
        }
        "
    done
    unset distro
fi

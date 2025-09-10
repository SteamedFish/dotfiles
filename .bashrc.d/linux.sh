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
if [ -n "$(command -v wl-copy)" ] && [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    alias pbcopy='wl-copy'
    alias pbpaste='wl-paste'
else
    alias pbcopy='xclip -selection c -i'
    alias pbpaste='xclip -selection c -o'
fi

for dir in "/var/lib/snapd/desktop" "/var/lib/flatpak/exports/share" "$HOME/.local/share/flatpak/exports/share"; do
    if [ -d "$dir" ]; then
        export XDG_DATA_DIRS="$XDG_DATA_DIRS:$dir"
    fi
done
unset dir

export SYSTEMD_LESS="FRXMK"

if [ -S "${XDG_RUNTIME_DIR}/ssh-agent.socket" ]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
fi

GPG_TTY=$(tty)
export GPG_TTY

# by default Linux don't show pings that don't get replied
# we want to show that
if [ -n "$(command -v grc)" ] && $CLICOLOR; then
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

if [ -n "$(command -v systemctl)" ]; then
    # This will not affect systemd services started before PATH is imported.
    systemctl --user import-environment PATH
fi

# nvm
if [ -f "/usr/share/nvm/init-nvm.sh" ]; then
    source /usr/share/nvm/init-nvm.sh
fi

# rootless docker
if [ -S "$XDG_RUNTIME_DIR/containerd-rootless/api.sock" ] && [ -n "$(command -v nerdctl)" ]; then
    # nerdctl detects rootless docker automatically
    :
elif [ -S "$XDG_RUNTIME_DIR/docker.sock" ]; then
    export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock
fi

# ROCm support for special GPUs
if [ -n "$(command -v rocminfo)" ]; then
    if rocminfo | grep -qw gfx1150; then # Radeon 890M
        # This is confirmed not working with HX370
        export HSA_OVERRIDE_GFX_VERSION=11.5.1
        export PYTORCH_ROCM_ARCH=gfx1151
    elif rocminfo | grep -qw gfx1103; then # Radeon 780M
        # This is confirmed working with 8845HS
        export HSA_OVERRIDE_GFX_VERSION=11.0.2
        export PYTORCH_ROCM_ARCH=gfx1102
    fi
fi

if [ -d "/opt/rocm" ]; then
    export ROCM_PATH=/opt/rocm
fi

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

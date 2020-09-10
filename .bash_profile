#!/bin/bash

# shellcheck source=.bashrc
[[ -f "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

if [ -n "$(command -v neofetch)" ];then
    neofetch
elif [ -n "$(command -v archey)" ];then
    archey -c -o
fi

if [ "${OSTYPE}" != "linux-android" ]; then
    return
fi

if [ -n "$(command -v toilet)" ]; then
    toilet --rainbow -w 300 -f ascii9 SteamedFish
elif [ -n "$(command -v figlet)" ]; then
    figlet -f mini -w 300 SteamedFish
fi

# code generated by script, ignore all shellcheck warnings
# shellcheck disable=2086,1091
test -e ${HOME}/.iterm2_shell_integration.bash && source ${HOME}/.iterm2_shell_integration.bash
# shellcheck disable=2086,1091
if [ -e ${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ${HOME}/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

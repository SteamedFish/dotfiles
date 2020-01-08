#!/bin/bash

[[ -f "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

if [ -n "$(command -v figlet)" -a "$(uname -o 2>&1)" != "Android" ]; then
    figlet -f mini -w 300 SteamedFish
fi

if [ -n "$(command -v neofetch)" ];then
    neofetch
elif [ -n "$(command -v archey)" ];then
    archey -c -o
fi


test -e ${HOME}/.iterm2_shell_integration.bash && source ${HOME}/.iterm2_shell_integration.bash
if [ -e ${HOME}/.nix-profile/etc/profile.d/nix.sh ]; then . ${HOME}/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

#!/bin/bash

# do nothing if not running interactively
case $- in
    *i*) ;;
    *) return;;
esac

# if [ -n "$(command -v toilet)" ]; then
#     if [[ "$OSTYPE" == "darwin"* ]] || [[ "$OSTYPE" == "linux-android" ]]; then
#         toilet -F gay -w 300 -f ascii9 SteamedFish
#     else
#         toilet -F rainbow -w 300 -f ascii9 SteamedFish
#     fi
# elif [ -n "$(command -v figlet)" ]; then
#     figlet -f mini -w 300 SteamedFish
# fi

if [ -n "$(command -v neofetch)" ];then
    neofetch
elif [ -n "$(command -v archey)" ];then
    archey -c -o
fi

# Added by OrbStack: command-line tools and integration
[ -f "$HOME/.orbstack/shell/init.zh" ] && source $HOME/.orbstack/shell/init.zsh 2>/dev/null || :

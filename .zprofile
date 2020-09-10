#!/bin/bash

if [ -n "$(command -v neofetch)" ];then
    neofetch
elif [ -n "$(command -v archey)" ];then
    archey -c -o
fi

if [[ "${OSTYPE}" == "linux-android" ]]; then
    return
fi

if [ -n "$(command -v toilet)" ]; then
    toilet --rainbow -w 300 -f ascii9 SteamedFish
elif [ -n "$(command -v figlet)" ]; then
    figlet -f mini -w 300 SteamedFish
fi

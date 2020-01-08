#!/bin/bash

if [ -n "$(command -v figlet)" -a "$(uname -o 2>&1)" != "Android" ]; then
    figlet -f mini -w 300 SteamedFish
fi

if [ -n "$(command -v neofetch)" ];then
    neofetch
elif [ -n "$(command -v archey)" ];then
    archey -c -o
fi

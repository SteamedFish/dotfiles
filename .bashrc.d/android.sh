#!/bin/bash


if [ -z "$TERMUX_VERSION" ]; then
    return 
fi	

if [ -n "$(command -v ssh-agent)" ]; then
    eval "$(ssh-agent)"
fi

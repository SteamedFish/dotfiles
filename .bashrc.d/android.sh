#!/bin/bash


if [ -z "$TERMUX_VERSION" ]; then
    return 
fi	

if [ -n "$(command -v ssh-agent)" ]; then
    eval "$(ssh-agent)"
fi

# redirect any GUI apps to XServer XSDL
# requires to install https://play.google.com/store/apps/details?id=x.org.server
export DISPLAY=localhost:0

#!/usr/bin/env bash

# WSL
if [[ "$(uname -r)" != *"-Microsoft" ]]; then
    return
fi

export DISPLAY=:0

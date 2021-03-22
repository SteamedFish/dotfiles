#!/usr/bin/env bash

# starship

if [[ "$TERM" == "dumb" ]]; then
    return
fi

if [ -z "$(command -v starship)" ];then
    return
fi

if [ -n "$BASH" ]; then
    eval "$(starship init bash)"
elif [[ "$(basename "$SHELL")" == "zsh" ]]; then
    eval "$(starship init zsh)"
fi

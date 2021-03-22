#!/usr/bin/env bash

# bash-powerline

if [ -n "$(command -v starship)" ];then
    return
fi

if [ -z "$BASH" ]; then
    return
fi

if [ "$TERM" == "dumb" ]; then
    return
fi

if [ "$TERM_PROGRAM" != "iTerm.app" ] && [ "$OSTYPE" != "linux-gnu" ]; then
    return
fi

if [ -f /etc/debian_version ]; then
    _SITE_PACKAGES="/usr/share/"
else
    _SITE_PACKAGES=$(python -c "import sys; print(next(p for p in sys.path if 'site-packages' in p and '.local' not in p))")
fi

# shellcheck disable=2034
if [ -f "$_SITE_PACKAGES"/powerline/bindings/bash/powerline.sh ]; then
    powerline-daemon -q
    POWERLINE_BASH_CONTINUATION=1
    POWERLINE_BASH_SELECT=1
    # shellcheck disable=1090
    . "$_SITE_PACKAGES"/powerline/bindings/bash/powerline.sh
fi
unset _SITE_PACKAGES

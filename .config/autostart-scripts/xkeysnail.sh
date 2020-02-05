#!/usr/bin/env bash

if [ -f /usr/bin/xkeysnail ] && [ -f "$HOME/.config/xkeysnail/config.py" ]; then
    /usr/bin/sudo /usr/bin/xkeysnail --quiet --watch "$HOME/.config/xkeysnail/config.py"
fi

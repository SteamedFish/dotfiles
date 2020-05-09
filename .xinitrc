#!/bin/bash

if [ -n "$(command -v xrdb)" ]; then
    xrdb -merge ~/.Xresources
fi

#!/usr/bin/env bash

if [[ "$OSTYPE" != "darwin"* ]]; then
    return
fi

# export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.cloud.tencent.com/homebrew-bottles

export HOMEBREW_BAT=true
export HOMEBREW_CURL_RETRIES=10
export HOMEBREW_NO_AUTO_UPDATE=true
export HOMEBREW_NO_INSTALL_CLEANUP=true
export HOMEBREW_UPDATE_REPORT_ONLY_INSTALLED=true

# envs from pass
if [ -n "$(command -v pass)" ]; then
    HOMEBREW_GITHUB_API_TOKEN="$(pass env/homebrew)"
    export HOMEBREW_GITHUB_API_TOKEN
fi

if [ -n "$(command -v grc)" ]; then
    if [ -n "$(command -v ip)" ]; then
        # mac by default don't have ip command
        # but we may install iproute2mac to have ip
        alias ip='grc --colour=auto ip'
    fi
fi

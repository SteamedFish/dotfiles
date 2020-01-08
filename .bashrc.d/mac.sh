#!/usr/bin/env bash

if [[ "$OSTYPE" != "darwin"* ]]; then
    return
fi

# export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.cloud.tencent.com/homebrew-bottles

# envs from pass
if [ -n "$(command -v pass)" ]; then
    export HOMEBREW_GITHUB_API_TOKEN="$(pass env/homebrew)"
fi

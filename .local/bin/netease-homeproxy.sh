#!/bin/bash
ssh -D 2080 -qfCNnT i.steamedfish.org

if [[ "$OSTYPE" == "darwin"* ]]; then
    for networkservice in Ethernet Wi-Fi; do
        # This must be a valid URL. macOS doesn't support file based pac address.
        networksetup -setautoproxyurl $networkservice https://steamedfish.org/netease-homeproxy.pac > /dev/null 2>&1 || true
        networksetup -setautoproxystate $networkservice on > /dev/null 2>&1 || true
    done
fi

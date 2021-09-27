#!/bin/bash

# automatically export proxy settings
for _networkservice in Ethernet Wi-Fi; do
    _socks_proxy_enabled=0
    if $(networksetup -getsocksfirewallproxy ${_networkservice} | grep -q 'Enabled: Yes'); then
        _socks_proxy_enabled=1
    fi
done
if [ "$_socks_proxy_enabled" -eq 1 ]; then
    export http_proxy=http://127.0.0.1:1087
    export https_proxy=http://127.0.0.1:1087
else
    unset http_proxy
    unset https_proxy
fi

unset _networkservice _socks_proxy_enabled

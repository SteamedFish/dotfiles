var proxy = 'SOCKS5 127.0.0.1:2080; SOCKS 127.0.0.1:2080; DIRECT;';
var direct = 'DIRECT;';

function FindProxyForURL(url, host) {
    //if (isInNet(host, "192.168.1.0", "255.255.255.0"))
    if (shExpMatch(host, "192.168.1.*")) {
        return proxy;
    }
    return direct;
}

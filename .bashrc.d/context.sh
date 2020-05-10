#!/usr/bin/env bash

# ConTeXt minimals
if [ ! -d "$HOME/ConTeXt" ] &&  [ ! -f '/opt/context-minimals/setuptex' ]; then
    return
fi

if [[ "$OSTYPE" = "darwin"* ]]; then
    if ! [ -f '/etc/paths.d/ConTeXt' ]; then
        echo "please run sudo echo $HOME/ConTeXt/tex/texmf-osx-64/bin > /etc/paths.d/ConTeXt"
    fi
    export OSFONTDIR="/Library/Fonts/;/System/Library/Fonts/;$HOME/Library/Fonts/"
elif [ "$OSTYPE" = "linux-gnu" ]; then
    if [ -f '/opt/context-minimals/setuptex' ]; then
        # context-minimals-git in ArchLinux
        export PATH="$PATH:/opt/context-minimals/texmf-linux-64/bin"
    else
        export PATH="$PATH:$HOME/ConTeXt/tex/texmf-linux-64/bin"
    fi
    export OSFONTDIR="/usr/share/fonts;$HOME/.fonts"
fi

#!/usr/bin/env bash

# ConTeXt minimals
if [ ! -d "$HOME/ConTeXt" ]; then
    return
fi

if [[ "$OSTYPE" = "darwin"* ]]; then
    if ! [ -f '/etc/paths.d/ConTeXt' ]; then
        echo "please run sudo echo $HOME/ConTeXt/tex/texmf-osx-64/bin > /etc/paths.d/ConTeXt"
    fi
    export OSFONTDIR="/Library/Fonts/;/System/Library/Fonts/;$HOME/Library/Fonts/"
elif [ "$OSTYPE" = "linux-gnu" ]; then
    export PATH="$PATH:$HOME/ConTeXt/tex/texmf-linux-64/bin"
    export OSFONTDIR="/usr/share/fonts;$HOME/.fonts"
fi

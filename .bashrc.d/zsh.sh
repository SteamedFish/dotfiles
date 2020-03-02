#!/usr/bin/env bash

if [ "$(basename "$SHELL")" != "zsh" ]; then
    return
fi

if [ -n "$BASH" ]; then
    return
fi

# type dir name without cd
setopt NO_AUTO_CD
# list completions the bash way
setopt BASH_AUTO_LIST
# do not give bad glob pattern error
setopt NO_BAD_PATTERN
# ask to correct commands
setopt CORRECT
# ask to correct all arguments
setopt NO_CORRECT_ALL

# History
export HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
export HISTSIZE=20000
export SAVEHIST=10000

# add more information to history
setopt EXTENDED_HISTORY
# expire duplicates first
setopt HIST_EXPIRE_DUPS_FIRST
# ignore dups when searching
setopt HIST_FIND_NO_DUPS
# do not store duplications
setopt HIST_IGNORE_ALL_DUPS
# removes blank lines
setopt HIST_REDUCE_BLANKS
# verity commands with subsitutions like !!
setopt HIST_VERIFY
# share history across sessions
setopt SHARE_HISTORY

# Correctly display UTF-8 with combining characters.
setopt COMBINING_CHARS

# emacs keys like C-a C-e
setopt EMACS

# suffix alias: how to open files with extension
alias -s log="nvim"

for _ext in txt yml yaml py sh el md org diff patch go sql h c cc cpp; do
    # shellcheck disable=2139
    alias -s "${_ext}=emacs"
done
unset _ext

if [ -f "$HOME/.zplugin/bin/zplugin.zsh" ]; then
    # shellcheck source=../.zplugin/bin/zplugin.zsh
    source "$HOME/.zplugin/bin/zplugin.zsh"
    zplugin ice wait blockf atpull'zplugin creinstall -q .'
    zplugin light zsh-users/zsh-completions

    zplugin ice wait atinit"zpcompinit; zpcdreplay"
    zplugin light zdharma/fast-syntax-highlighting

    zplugin ice wait atload"_zsh_autosuggest_start"
    zplugin light zsh-users/zsh-autosuggestions

    zplugin ice wait lucid
    zplugin load hlissner/zsh-autopair

    zplugin ice wait"1" lucid
    zplugin load psprint/zsh-navigation-tools

    zplugin ice wait'!' lucid atload'source ~/.p10k.zsh; _p9k_precmd' nocd
    zplugin light romkatv/powerlevel10k
fi

# shellcheck disable=1090
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

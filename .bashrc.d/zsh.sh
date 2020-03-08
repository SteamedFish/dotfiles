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

if [ -f "$HOME/.zinit/bin/zinit.zsh" ]; then
    # shellcheck source=../.zinit/bin/zinit.zsh
    source "$HOME/.zinit/bin/zinit.zsh"
    zinit ice wait blockf atpull'zinit creinstall -q .'
    zinit light zsh-users/zsh-completions

    #zinit ice wait atinit"zpcompinit; zpcdreplay"
    #zinit light zdharma/fast-syntax-highlighting

    zinit ice wait atload"_zsh_autosuggest_start"
    zinit light zsh-users/zsh-autosuggestions

    zinit ice wait lucid
    zinit load hlissner/zsh-autopair

    zinit ice wait"1" lucid
    zinit load psprint/zsh-navigation-tools

    zinit ice wait'!' lucid atload'source ~/.p10k.zsh; _p9k_precmd' nocd
    zinit light romkatv/powerlevel10k
fi

# shellcheck disable=1090
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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
# don't kill background jobs
setopt NO_HUP

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

for _ext in txt yml yaml el md org diff patch go sql h c cc cpp; do
    # shellcheck disable=2139
    alias -s "${_ext}=emacs"
done
unset _ext

if [ -f "$HOME/.zinit/bin/zinit.zsh" ]; then
    # shellcheck source=../.zinit/bin/zinit.zsh
    source "$HOME/.zinit/bin/zinit.zsh"

    if [ -z "$(command -v starship)" ]; then
        zinit light romkatv/powerlevel10k
    fi

    zinit ice wait blockf atpull'zinit creinstall -q .'
    zinit light zsh-users/zsh-completions

    zinit ice wait atinit"zpcompinit; zpcdreplay"
    zinit light zdharma-continuum/fast-syntax-highlighting

    zinit ice wait atload"_zsh_autosuggest_start"
    zinit light zsh-users/zsh-autosuggestions

    zinit ice wait lucid
    zinit load hlissner/zsh-autopair

    if [ -z "$(command -v atuin)" ]; then
        zinit ice wait"1" lucid
        zinit load z-shell/zsh-navigation-tools
    fi

    if [[ "$OSTYPE" == "darwin"* ]]; then
        zinit light iam4x/zsh-iterm-touchbar
    fi

    # by default wakatime-zsh-plugin searches for binary wakatime-cli
    # but wakatime-cli may install just wakatime
    if [ -n "$(command -v wakatime)" ]; then
        export ZSH_WAKATIME_BIN=$(command -v wakatime)
    fi
    zinit light sobolevn/wakatime-zsh-plugin

fi

# kubecolor borrows kubectl's completion. zsh expands `kubectl` (alias) before
# dispatch, so the completion for `kubecolor` itself must be registered.
# compdef only exists after compinit, which zinit loads later (wait), so defer
# to the first precmd that can see kubectl's completion.
if (( $+commands[kubecolor] )); then
    _register_kubecolor_completion() {
        (( ${+_comps[kubectl]} )) || return 0
        (( ${+_comps[kubecolor]} )) || compdef kubecolor=kubectl
        add-zsh-hook -d precmd _register_kubecolor_completion
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd _register_kubecolor_completion
fi

# shellcheck disable=1090
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

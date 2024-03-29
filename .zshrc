#!/usr/bin/zsh
#
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

test -f /etc/profile && source /etc/profile
if [ -n "$TERMUX_VERSION" ]; then
    test -f "$PREFIX/etc/profile" && source "$PREFIX/etc/profile"
fi	
source "$HOME/.bashrc"

# do nothing if not running interactively
case $- in
    *i*) ;;
    *) return;;
esac

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

if [ -z "$(command -v starship)" ];then
    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
fi


HISTFILE=$HOME/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt SHARE_HISTORY

bindkey -e
setopt interactivecomments

autoload -U select-word-style
select-word-style bash

alias tmux='tmux -2'
alias mc='SHELL=bash mc'

export EDITOR='emacs -nw --no-desktop'
export DIFF='ediff -nw --no-desktop'

export LC_ALL=ru_RU.UTF-8
export LESSCHARSET=utf-8

export ZSH="$HOME/.oh-my-zsh"

PROMPT="$ "

plugins=(
    git
    history
)

source $ZSH/oh-my-zsh.sh

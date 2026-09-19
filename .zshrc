HISTFILE=$HOME/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt SHARE_HISTORY

bindkey -e
setopt interactivecomments

autoload -U select-word-style
select-word-style bash

autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

alias tmux='tmux -2'
alias mc='SHELL=bash mc'

export EDITOR='emacs -nw --no-desktop'
export DIFF='ediff -nw --no-desktop'

export LC_ALL=ru_RU.UTF-8
export LESSCHARSET=utf-8

PROMPT="$ "

# apt puts these in /usr/share, brew in $(brew --prefix)/share
for plugin in zsh-autosuggestions zsh-syntax-highlighting
do
    source ${HOMEBREW_PREFIX:-/usr}/share/$plugin/$plugin.zsh
done
ZSH_HIGHLIGHT_STYLES[comment]='fg=gray'

# fzf: Ctrl-R history, Ctrl-T files, Alt-C cd, **<Tab> fuzzy completion
source <(fzf --zsh)

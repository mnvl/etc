
export PS1='$ '

source ~/etc/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/etc/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/etc/zsh-history-substring-search/zsh-history-substring-search.zsh

fpath=(~/etc/zsh-completions $fpath)
compinit

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

HISTFILE=$HOME/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt SHARE_HISTORY

bindkey -e
setopt interactivecomments

autoload -U select-word-style
select-word-style bash

alias tmux='tmux -2'

SSHX_PORTS=`awk 'BEGIN { for (i = 7000; i <= 7010; i++) printf " -L %d:127.0.0.1:%d", i, i }'`
alias sshx="ssh -X $SSHX_PORTS"

export EDITOR='emacs -nw --no-desktop'
export DIFF='ediff -nw --no-desktop'

export PATH=~/.local/bin:/usr/local/bin/:${PATH:+:${PATH}}

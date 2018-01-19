
# git clone https://github.com/powerline/fonts.git --depth=1 && cd fonts && ./install.sh

zstyle ':prezto:load' pmodule \
       'environment' \
       'terminal' \
       'editor' \
       'history' \
       'directory' \
       'spectrum' \
       'utility' \
       'prompt' \
       'completion' \
       'history-substring-search' \
       'autosuggestions' \
       'syntax-highlighting'

zstyle ':prezto:module:syntax-highlighting' highlighters \
       'main' \
       'brackets' \
       'pattern' \
       'line' \
       'root'

zstyle ':prezto:*:*' case-sensitive 'yes'
zstyle ':prezto:*:*' color 'yes'

zstyle ':prezto:module:prompt' theme 'powerlevel9k'
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator vcs virtualenv background_jobs history time)

zstyle ':prezto:module:terminal' auto-title 'yes'
zstyle ':prezto:module:terminal:window-title' format '%n@%m: %s'
zstyle ':prezto:module:terminal:tab-title' format '%m: %s'
zstyle ':prezto:module:terminal:multiplexer-title' format '%s'

source ~/etc/prezto/init.zsh

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

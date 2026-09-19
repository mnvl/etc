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

alias mc='SHELL=bash mc'
if command -v eza >/dev/null; then
    alias ls='eza'
    alias ll='eza -l'
    alias la='eza -la'
fi
# Debian/Ubuntu install bat/fd under different names
command -v batcat >/dev/null && alias bat='batcat'
command -v fdfind >/dev/null && alias fd='fdfind'

export EDITOR='emacs -nw --no-desktop'
export DIFF='ediff -nw --no-desktop'

# LANG for locale, but keep sort order / number formatting / messages in C
export LANG=ru_RU.UTF-8
export LC_COLLATE=C
export LC_NUMERIC=C
export LC_MESSAGES=C
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

# zoxide: z <dir> jumps to a frecent directory, zi picks interactively
command -v zoxide >/dev/null && eval "$(zoxide init zsh)"

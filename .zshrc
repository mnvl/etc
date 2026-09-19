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

# file colors for ls/eza/completion menu
command -v dircolors >/dev/null && eval "$(dircolors -b)"
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''

alias mc='SHELL=bash mc'
if command -v eza >/dev/null; then
    alias ls='eza --group-directories-first'
    alias ll='eza -l --group-directories-first --git'
    alias la='eza -la --group-directories-first --git'
fi
# Debian/Ubuntu install bat/fd under different names
command -v batcat >/dev/null && alias bat='batcat'
command -v fdfind >/dev/null && alias fd='fdfind'
export BAT_THEME='Monokai Extended'
export LESS='-R'
export MANPAGER="sh -c 'col -bx | $(command -v batcat || echo bat) -l man -p'"

export EDITOR='emacs -nw --no-desktop'
export DIFF='ediff -nw --no-desktop'

# LANG for locale, but keep sort order / number formatting / messages in C
export LANG=ru_RU.UTF-8
export LC_COLLATE=C
export LC_NUMERIC=C
export LC_MESSAGES=C
export LESSCHARSET=utf-8

# starship draws a gray info line (dir, git, duration, exit code) above the plain "$ " prompt
# see config/starship.toml
export STARSHIP_CONFIG=$HOME/etc/config/starship.toml
if command -v starship >/dev/null; then
    eval "$(starship init zsh)"
else
    PROMPT="$ "
fi

# apt puts these in /usr/share, brew in $(brew --prefix)/share
for plugin in zsh-autosuggestions zsh-syntax-highlighting
do
    source ${HOMEBREW_PREFIX:-/usr}/share/$plugin/$plugin.zsh
done
# monokai: green commands, yellow strings, purple numbers-ish, gray comments, red errors
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
ZSH_HIGHLIGHT_STYLES[comment]='fg=#75715e'
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=#f92672'
ZSH_HIGHLIGHT_STYLES[command]='fg=#a6e22e'
ZSH_HIGHLIGHT_STYLES[builtin]='fg=#a6e22e'
ZSH_HIGHLIGHT_STYLES[alias]='fg=#a6e22e'
ZSH_HIGHLIGHT_STYLES[function]='fg=#a6e22e'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=#a6e22e,underline'
ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=#f92672'
ZSH_HIGHLIGHT_STYLES[path]='fg=#66d9ef'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=#ae81ff'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=#e6db74'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=#e6db74'
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=#e6db74'
ZSH_HIGHLIGHT_STYLES[redirection]='fg=#f92672'
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=#fd971f'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=#fd971f'
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#75715e'

# fzf: Ctrl-R history, Ctrl-T files, Alt-C cd, **<Tab> fuzzy completion
source <(fzf --zsh)
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border \
    --color=bg+:#3e3d32,bg:#272822,spinner:#a6e22e,hl:#f92672 \
    --color=fg:#f8f8f2,header:#66d9ef,info:#e6db74,pointer:#a6e22e \
    --color=marker:#a6e22e,fg+:#f8f8f2,prompt:#66d9ef,hl+:#f92672"
export FZF_CTRL_T_OPTS="--preview '$(command -v batcat || echo bat) --color=always --style=numbers --line-range=:200 {}'"

# zoxide: z <dir> jumps to a frecent directory, zi picks interactively
command -v zoxide >/dev/null && eval "$(zoxide init zsh)"

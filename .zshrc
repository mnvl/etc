HISTFILE=$HOME/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt SHARE_HISTORY INC_APPEND_HISTORY EXTENDED_HISTORY
setopt HIST_IGNORE_ALL_DUPS HIST_IGNORE_SPACE

bindkey -e
setopt interactivecomments

# Homebrew: sets PATH and HOMEBREW_PREFIX; used on Linux too when it is there
# (same list of prefixes as install.sh)
for brew in /opt/homebrew/bin/brew /usr/local/bin/brew \
            /home/linuxbrew/.linuxbrew/bin/brew ~/.linuxbrew/bin/brew
do
    [[ -x $brew ]] && eval "$($brew shellenv)" && break
done

autoload -U select-word-style
select-word-style bash

autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# file colors for eza/fd/completion menu; vivid works on macOS too, where there is no
# dircolors, and is not packaged for Debian/Ubuntu, where dircolors is the fallback
if command -v vivid >/dev/null; then
    export LS_COLORS="$(vivid generate molokai)"
elif command -v dircolors >/dev/null; then
    eval "$(dircolors -b)"
fi
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''

alias mc='SHELL=bash mc'
if command -v eza >/dev/null; then
    alias ls='eza --group-directories-first'
    alias ll='eza -l --group-directories-first --git'
    alias la='eza -la --group-directories-first --git'
fi
# brew installs bat/fd under their real names; Debian renames them, so alias only
# when there is no real one (an apt bat/fd left over must not shadow brew's newer one)
command -v bat >/dev/null || { command -v batcat >/dev/null && alias bat='batcat' }
command -v fd  >/dev/null || { command -v fdfind >/dev/null && alias fd='fdfind' }
# whence -p, not command -v: the alias just defined above would shadow the binary
bat_bin=$(whence -p bat || whence -p batcat || true)
export LESS='-R'
# without bat, leave MANPAGER alone: man's own pager beats a broken pipeline
[[ -n $bat_bin ]] && export MANPAGER="sh -c 'col -bx | $bat_bin -l man -p'"

export EDITOR='emacs -nw --no-desktop'

# LANG for locale, but keep sort order / number formatting / messages in C
export LANG=ru_RU.UTF-8
export LC_COLLATE=C
export LC_NUMERIC=C
export LC_MESSAGES=C
export LESSCHARSET=utf-8

# starship draws a gray info line (dir, git, duration, exit code) above the plain "$ " prompt
# see config/starship.toml (linked to ~/.config/starship.toml by link.sh)
if command -v starship >/dev/null; then
    eval "$(starship init zsh)"
else
    PROMPT="$ "
fi

# brew keeps these in $HOMEBREW_PREFIX/share; a distro package would be in /usr/share
for plugin in zsh-autosuggestions zsh-syntax-highlighting
do
    for dir in ${HOMEBREW_PREFIX:-/usr}/share /usr/share
    do
        [[ -r $dir/$plugin/$plugin.zsh ]] && source $dir/$plugin/$plugin.zsh && break
    done
done

# fzf: Ctrl-R history, Ctrl-T files, Alt-C cd, **<Tab> fuzzy completion.
# --zsh only exists since 0.48; older ones (Debian/Ubuntu) ship the scripts as docs.
if command -v fzf >/dev/null; then
    if fzf --zsh >/dev/null 2>&1; then
        source <(fzf --zsh)
    else
        for f in /usr/share/doc/fzf/examples/{key-bindings,completion}.zsh
        do
            [[ -r $f ]] && source $f
        done
    fi
fi

[[ -n $bat_bin ]] && \
    export FZF_CTRL_T_OPTS="--preview '$bat_bin --color=always --style=numbers --line-range=:200 {}'"

# zoxide: z <dir> jumps to a frecent directory, zi picks interactively
command -v zoxide >/dev/null && eval "$(zoxide init zsh)"

#! /bin/sh -eu

# Symlinks dotfiles from ~/etc into $HOME. Safe to re-run; needs no sudo.

cd "$HOME/etc"

for x in .emacs .gdbinit .tmux.conf .gitconfig .zshrc .zshenv
do
    ln -f -s "$HOME/etc/$x" "$HOME/$x"
done

mkdir -p "$HOME/.claude"
ln -f -s "$HOME/etc/claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"

if [ "$(uname -s)" = "Darwin" ] || [ -n "${DISPLAY:-}" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
    case "$(uname -s)" in
        Darwin*) vscode_dir="$HOME/Library/Application Support/Code/User" ;;
        *)       vscode_dir="$HOME/.config/Code/User"
                 mkdir -p "$HOME/.config/keyd" "$HOME/.config/autostart"
                 ln -f -s "$HOME/etc/keyd/app.conf" "$HOME/.config/keyd/app.conf"
                 ln -f -s "$HOME/etc/keyd/keyd-application-mapper.desktop" "$HOME/.config/autostart/keyd-application-mapper.desktop"
                 ;;
    esac
    mkdir -p "$vscode_dir"
    for x in "$HOME/etc"/vscode/*.json
    do
        ln -f -s "$x" "$vscode_dir/"
    done
fi

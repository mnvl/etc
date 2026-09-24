#! /bin/sh -eux

cd "$HOME/etc"

for x in .emacs .gdbinit .tmux.conf .gitconfig .zshrc
do
    ln -f -s "$HOME/etc/$x" "$HOME/$x"
done

if [ ! -f "$HOME/.zshenv" ];
then
    cp .zshenv "$HOME/.zshenv"
fi

mkdir -p "$HOME/.claude"
ln -f -s "$HOME/etc/claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"

# config/<name> -> ~/.config/<name>; -n so an existing directory is replaced, not linked into
mkdir -p "$HOME/.config"
for x in "$HOME/etc"/config/*
do
    ln -f -s -n "$x" "$HOME/.config/$(basename "$x")"
done

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

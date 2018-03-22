#! /bin/sh -ex

for x in .zshrc .emacs .jupyter .gdbinit .tmux.conf
do
    ln -f -s ~/etc/$x ~/$x
done

for x in keybindings.json settings.json
do
    ln -f -s ~/etc/vscode/$x ~/.config/Code/User/$x
done

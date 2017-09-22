#! /bin/sh -ex

for x in .zshrc .emacs .jupyter .gdbinit .tmux.conf
do
    ln -f -s ~/etc/$x ~/
done

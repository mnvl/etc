#! /bin/sh -ex

sh -c "CHSH=no RUNZSH=no KEEP_ZSHRC=yes $(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

for x in .zshrc .emacs .jupyter .gdbinit .tmux.conf .gitconfig
do
    ln -f -s ~/etc/$x ~/$x
done

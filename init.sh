#! /bin/sh -ex

case "$(uname -s)" in
    Linux*)
        sudo apt-get install fish mc emacs tmux clangd git-gui
    ;;

    Darwin*)
        brew install fish mc emacs tmux llvm git-gui
    ;;

    *)
        echo "unknown OS"
        exit 1
esac

rm -rf $HOME/.tmux/plugins/tpm
mkdir -p ~/.tmux/plugins
git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm

for x in .emacs .jupyter .gdbinit .tmux.conf .gitconfig
do
    ln -f -s $HOME/etc/$x $HOME/$x
done

ln -f -s $HOME/etc/fish $HOME/.config/fish

for x in $HOME/etc/vscode/*.json
do
    ln -f -s $x $HOME/.config/Code/User/
done

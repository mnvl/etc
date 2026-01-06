#! /bin/sh -ex

curl -s 'https://api.github.com/repos/be5invis/Iosevka/releases/latest' | jq -r ".assets[] | .browser_download_url" | grep 'PkgTTC-Iosevka-.*zip' | xargs -n 1 curl -L -O --fail --silent --show-error
unzip PkgTTC-Iosevka-*.zip

case "$(uname -s)" in
    Linux*)
        gsettings set org.gnome.desktop.wm.preferences mouse-button-modifier ''
        gsettings set org.gnome.shell.extensions.dash-to-dock always-center-icons true

        sudo apt-get install fish mc emacs tmux clangd git-gui git-lfs

        cp *.ttc ~/.fonts
        fc-cache
    ;;

    Darwin*)
        brew install fish mc emacs tmux llvm git-gui
        brew install --cask font-iosevka
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

#! /bin/sh -ex

# Detect whether a GUI is available
has_gui=false
if [ "$(uname -s)" = "Darwin" ]; then
    has_gui=true
elif [ -n "$DISPLAY" ] || [ -n "$WAYLAND_DISPLAY" ]; then
    has_gui=true
fi

if $has_gui && test ! -e PkgTTC-Iosevka-*.zip
then
   curl -s 'https://api.github.com/repos/be5invis/Iosevka/releases/latest' | jq -r ".assets[] | .browser_download_url" | grep 'PkgTTC-Iosevka-.*zip' | xargs -n 1 curl -L -O --fail --silent --show-error
   unzip PkgTTC-Iosevka-*.zip
fi

case "$(uname -s)" in
    Linux*)
        if $has_gui; then
            gsettings set org.gnome.desktop.wm.preferences mouse-button-modifier ''
            gsettings set org.gnome.shell.extensions.dash-to-dock always-center-icons true
        fi

        sudo apt-get install zsh fish mc emacs tmux clangd git-gui git-lfs fzf bat parallel fd-find

        if $has_gui; then
            cp *.ttc ~/.fonts
            fc-cache
        fi
    ;;

    Darwin*)
        brew install fish mc emacs tmux llvm git-gui fzf bat parallel
        brew install --cask font-iosevka
    ;;

    *)
        echo "unknown OS"
        exit 1
esac

rm -rf $HOME/.tmux/plugins/tpm
mkdir -p ~/.tmux/plugins
git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm

sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
cd ~/.oh-my-zsh/custom/plugins/
for plugin in zsh-autosuggestions zsh-completions zsh-history-substring-search zsh-syntax-highlighting
do
    git clone https://github.com/zsh-users/$plugin.git
done

for x in .emacs .jupyter .gdbinit .tmux.conf .gitconfig .zshrc
do
    ln -f -s $HOME/etc/$x $HOME/$x
done

ln -f -s $HOME/etc/fish $HOME/.config/fish

if $has_gui; then
    case "$(uname -s)" in
        Darwin*) vscode_dir="$HOME/Library/Application Support/Code/User" ;;
        *)       vscode_dir="$HOME/.config/Code/User" ;;
    esac
    for x in $HOME/etc/vscode/*.json
    do
        ln -f -s "$x" "$vscode_dir/"
    done
fi

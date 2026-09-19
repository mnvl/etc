#! /bin/sh -eux

has_gui=false
if [ "$(uname -s)" = "Darwin" ] || [ -n "${DISPLAY:-}" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
    has_gui=true
fi

case "$(uname -s)" in
    Linux*)
        # bat and fd are installed as batcat / fdfind on Debian/Ubuntu; .zshrc aliases them
        sudo apt-get install -y zsh zsh-autosuggestions zsh-syntax-highlighting \
            mc emacs tmux clangd git git-lfs git-delta lazygit \
            fzf bat fd-find ripgrep eza zoxide parallel btop starship

        if $has_gui; then
            sudo apt-get install -y keyd keyd-application-mapper
            sudo ln -f -s "$HOME/etc/keyd/default.conf" /etc/keyd/default.conf
            sudo systemctl enable keyd
            sudo systemctl restart keyd
            sudo usermod -aG keyd "$USER"

            mkdir -p "$HOME/.fonts"
            if ! ls "$HOME/.fonts"/Iosevka-*.ttc >/dev/null 2>&1; then
                tmp=$(mktemp -d)
                curl -s 'https://api.github.com/repos/be5invis/Iosevka/releases/latest' \
                    | jq -r '.assets[] | .browser_download_url' \
                    | grep 'PkgTTC-Iosevka-.*zip' \
                    | xargs -n 1 curl -L --fail --silent --show-error -o "$tmp/iosevka.zip"
                unzip -q "$tmp/iosevka.zip" -d "$HOME/.fonts"
                rm -rf "$tmp"
                fc-cache
            fi
        fi
    ;;

    Darwin*)
        # brew is not on PATH until .zshrc is linked (see the same loop there)
        for brew in /opt/homebrew/bin/brew /usr/local/bin/brew
        do
            [ -x "$brew" ] && eval "$($brew shellenv)" && break
        done
        brew install zsh-autosuggestions zsh-syntax-highlighting mc emacs tmux llvm git-lfs git-delta lazygit \
            fzf bat fd ripgrep eza zoxide parallel btop starship
        brew install --cask font-iosevka
    ;;

    *)
        echo "unknown OS"
        exit 1
esac

"$(dirname "$0")/link.sh"

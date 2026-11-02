#! /bin/sh -eux

os=$(uname -s)

has_gui=false
if [ "$os" = "Darwin" ] || [ -n "${DISPLAY:-}" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
    has_gui=true
fi

if ! zb --help
then
    curl -fsSL https://zerobrew.rs/install | bash
fi

zb install \
   bat \
   btop \
   emacs \
   eza \
   fd \
   fzf \
   git \
   git-delta \
   git-lfs \
   jq \
   lazygit \
   llvm \
   mc \
   nvtop \
   parallel \
   ripgrep \
   starship \
   tmux \
   vivid \
   yazi \
   zoxide \
   zsh \
   zsh-autosuggestions \
   zsh-syntax-highlighting

if $has_gui
then
    case "$os" in
        Darwin*)
            echo "zb --cask is not supported on MacOS yet"
            zb install --cask font-iosevka ghostty
        ;;

        Linux*)
            sudo apt install ghostty

            # keyd is a system daemon (systemd unit, /etc/keyd, a group): distro package only
            sudo apt install keyd keyd-application-mapper
            sudo ln -f -s "$HOME/etc/keyd/default.conf" /etc/keyd/default.conf
            sudo systemctl enable keyd
            sudo systemctl restart keyd
            sudo usermod -aG keyd "$USER"

            fonts="$HOME/.local/share/fonts"
            mkdir -p "$fonts"
            if ! ls "$fonts"/Iosevka-*.ttc >/dev/null 2>&1; then
                tmp=$(mktemp -d)
                curl -s 'https://api.github.com/repos/be5invis/Iosevka/releases/latest' \
                    | jq -r '.assets[] | .browser_download_url' \
                    | grep 'PkgTTC-Iosevka-.*zip' \
                    | xargs -n 1 curl -L --fail --silent --show-error -o "$tmp/iosevka.zip"
                unzip -q "$tmp/iosevka.zip" -d "$fonts"
                rm -rf "$tmp"
                fc-cache
            fi
        ;;
    esac
fi

"$(dirname "$0")/link.sh"

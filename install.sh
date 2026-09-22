#! /bin/sh -eux

os=$(uname -s)

has_gui=false
if [ "$os" = "Darwin" ] || [ -n "${DISPLAY:-}" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
    has_gui=true
fi

# Homebrew is the package manager on both macOS and Linux: Debian ships most of
# the list below too old, under a different name, or not at all (eza, atuin,
# starship, lazygit, git-delta, vivid).
find_brew() {
    for brew in /opt/homebrew/bin/brew /usr/local/bin/brew \
                /home/linuxbrew/.linuxbrew/bin/brew "$HOME/.linuxbrew/bin/brew"
    do
        # shellcheck disable=SC2015
        [ -x "$brew" ] && eval "$($brew shellenv)" && return 0
    done
    return 1
}

if ! find_brew
then
    if [ "$os" = "Linux" ]; then
        sudo apt-get update
        sudo apt-get install -y build-essential procps curl file git
    fi
    NONINTERACTIVE=1 /bin/bash -c \
        "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    find_brew
fi

brew install zsh-autosuggestions zsh-syntax-highlighting \
    mc emacs tmux llvm git git-lfs git-delta lazygit \
    fzf bat fd ripgrep eza zoxide vivid atuin parallel htop btop starship jq

# llvm is keg-only, so clangd/clang-format are not on PATH; eglot looks them up by name
for x in clangd clang-format
do
    ln -f -s "$(brew --prefix llvm)/bin/$x" "$(brew --prefix)/bin/$x"
done

if [ "$os" = "Linux" ]; then
    # Debian's zsh is fine, but keep one zsh so ${HOMEBREW_PREFIX}/share plugins match it
    brew install zsh
    zsh="$(brew --prefix)/bin/zsh"
    grep -q -x -F "$zsh" /etc/shells || echo "$zsh" | sudo tee -a /etc/shells >/dev/null
    [ "${SHELL:-}" = "$zsh" ] || echo "to make it the login shell: chsh -s $zsh"
fi

if $has_gui
then
    case "$os" in
        Darwin*)
            brew install --cask font-iosevka ghostty
        ;;

        Linux*)
            sudo apt install ghostty

            # keyd is a system daemon (systemd unit, /etc/keyd, a group): distro package only
            if sudo apt-get install -y keyd keyd-application-mapper
            then
                sudo ln -f -s "$HOME/etc/keyd/default.conf" /etc/keyd/default.conf
                sudo systemctl enable keyd
                sudo systemctl restart keyd
                sudo usermod -aG keyd "$USER"
            else
                echo "keyd is not packaged for this release - skipped"
            fi

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

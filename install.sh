#! /bin/sh -eux

os=$(uname -s)

has_gui=false
if [ "$os" = "Darwin" ] || [ -n "${DISPLAY:-}" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
    has_gui=true
fi

as_root() {
    if [ "$(id -u)" = 0 ]; then "$@"; else sudo "$@"; fi
}

# Homebrew is the package manager on macOS, and the better one on Linux: Debian
# ships part of the list below too old, under a different name, or not at all.
# It is used on Linux only when it is already there - it refuses to run as root,
# so a box where everybody is root cannot have one, and that is a supported case.
find_brew() {
    for brew in /opt/homebrew/bin/brew /usr/local/bin/brew \
                /home/linuxbrew/.linuxbrew/bin/brew "$HOME/.linuxbrew/bin/brew"
    do
        # shellcheck disable=SC2015
        [ -x "$brew" ] && eval "$($brew shellenv)" && return 0
    done
    return 1
}

if ! find_brew && [ "$os" = "Darwin" ]
then
    NONINTERACTIVE=1 /bin/bash -c \
        "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    find_brew || { echo "Homebrew install failed, and macOS has nothing else" >&2; exit 1; }
fi

# apt fallback: install what this release actually carries and name the rest, so
# a missing tool shows up here instead of as a broken shell later on.
have_pkg() { apt-cache show "$1" >/dev/null 2>&1; }

apt_install() {
    have=
    miss=
    for p in "$@"
    do
        if have_pkg "$p"; then have="$have $p"; else miss="$miss $p"; fi
    done
    # shellcheck disable=SC2086
    if [ -n "$have" ]; then as_root apt-get install -y --no-install-recommends $have; fi
    if [ -n "$miss" ]; then echo "skipped:$miss (not packaged for this release)"; fi
}

if command -v brew >/dev/null 2>&1
then
    brew install zsh-autosuggestions zsh-syntax-highlighting \
        mc emacs tmux llvm git git-lfs git-delta lazygit \
        fzf bat fd ripgrep eza zoxide vivid parallel htop btop starship jq

    # llvm is keg-only, so clangd/clang-format are not on PATH; eglot looks them up by name
    for x in clangd clang-format
    do
        ln -f -s "$(brew --prefix llvm)/bin/$x" "$(brew --prefix)/bin/$x"
    done

    if [ "$os" = "Linux" ]; then
        # Debian's zsh is fine, but keep one zsh so ${HOMEBREW_PREFIX}/share plugins match it
        brew install zsh
        zsh="$(brew --prefix)/bin/zsh"
        grep -q -x -F "$zsh" /etc/shells || echo "$zsh" | as_root tee -a /etc/shells >/dev/null
        [ "${SHELL:-}" = "$zsh" ] || echo "to make it the login shell: chsh -s $zsh"
    fi
elif [ "$os" = "Linux" ]
then
    as_root apt-get update

    # emacs pulls in the whole of X; a headless box only needs the terminal build.
    # eza/git-delta/lazygit/vivid/starship are unpackaged before Debian 13 /
    # Ubuntu 24.04 and are simply skipped - .zshrc and .gitconfig cope without them.
    if $has_gui; then emacs=emacs; else emacs=emacs-nox; fi
    apt_install zsh zsh-autosuggestions zsh-syntax-highlighting \
        mc "$emacs" tmux clangd clang-format git git-lfs git-delta lazygit \
        fzf bat fd-find ripgrep eza zoxide vivid parallel htop btop starship jq
else
    echo "no supported package manager for $os" >&2
    exit 1
fi

if $has_gui
then
    case "$os" in
        Darwin*)
            brew install --cask font-iosevka ghostty
        ;;

        Linux*)
            apt_install ghostty

            # keyd is a system daemon (systemd unit, /etc/keyd, a group): distro package only
            if have_pkg keyd && have_pkg keyd-application-mapper
            then
                apt_install keyd keyd-application-mapper
                as_root ln -f -s "$HOME/etc/keyd/default.conf" /etc/keyd/default.conf
                as_root systemctl enable keyd
                as_root systemctl restart keyd
                as_root usermod -aG keyd "$USER"
            else
                echo "skipped: keyd keyd-application-mapper (not packaged for this release)"
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

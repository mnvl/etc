#! /bin/sh -eux

# Linux only: zb has no --cask yet, so there is no macOS path here.
# Ubuntu 26.04+ packages everything but yazi; .zshrc copes with batcat/fdfind
# and finds the zsh plugins in /usr/share.

has_gui=false
if [ -n "${DISPLAY:-}" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
    has_gui=true
fi

# emacs pulls in the whole of X; a headless box only needs the terminal build
if $has_gui; then emacs=emacs; else emacs=emacs-nox; fi

sudo apt-get update
sudo apt-get install -y \
   bat \
   btop \
   clang-format \
   clangd \
   curl \
   "$emacs" \
   eza \
   fd-find \
   fzf \
   git \
   git-delta \
   git-lfs \
   jq \
   lazygit \
   mc \
   nvtop \
   parallel \
   ripgrep \
   starship \
   tmux \
   vivid \
   zoxide \
   zsh \
   zsh-autosuggestions \
   zsh-syntax-highlighting

if ! zb --help
then
    curl -fsSL https://zerobrew.rs/install | bash
fi

zb install yazi

if $has_gui
then
    sudo apt-get install -y ghostty fonts-iosevka

    # keyd is a system daemon (systemd unit, /etc/keyd, a group): distro package only
    sudo apt-get install -y keyd keyd-application-mapper
    sudo ln -f -s "$HOME/etc/keyd/default.conf" /etc/keyd/default.conf
    sudo systemctl enable keyd
    sudo systemctl restart keyd
    sudo usermod -aG keyd "$USER"

    # no zb (casks need 'binary' artifacts) or apt package for the nerd symbols
    fonts="$HOME/.local/share/fonts"
    mkdir -p "$fonts"
    if [ ! -e "$fonts/SymbolsNerdFont-Regular.ttf" ]; then
        curl -fsSL https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.tar.xz \
            | tar -xJf - -C "$fonts" SymbolsNerdFont-Regular.ttf SymbolsNerdFontMono-Regular.ttf
        fc-cache
    fi
fi

"$(dirname "$0")/link.sh"

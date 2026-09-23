git clone https://github.com/mnvl/etc.git ~/etc
~/etc/install.sh   # packages + fonts + symlinks
~/etc/link.sh      # symlinks only

Homebrew is the package manager, and install.sh bootstraps it on macOS. On Linux
it is only used when already present - it refuses to run as root - and otherwise
the install falls back to apt, which costs the tools Debian/Ubuntu do not package
before Debian 13 / Ubuntu 24.04: eza, git-delta, lazygit, vivid and starship are
skipped and named at the end of the run, and the dotfiles degrade to match.

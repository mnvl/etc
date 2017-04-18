
HISTFILE=$HOME/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt SHARE_HISTORY

bindkey -e

setopt interactivecomments

zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit

PS1='$ '

preexec () {
  [ -n "$STY" ] && echo -ne "\ek${1%% *}\e\\"
}

autoload -U select-word-style
select-word-style bash

alias tmux='tmux -2'

alias uri_escape='perl -MURI::Escape -e "while(<STDIN>) { print uri_escape(\$_) };"'
alias uri_unescape='perl -MURI::Escape -e "while(<STDIN>) { print uri_unescape(\$_) };"'
alias gplines="feedgnuplot --lines"
alias convert_all_flac_in_current_dir_to_mp3='for f in *.flac; do flac -cd "$f" | lame -b 320 -q 0 - "${f%.*}".mp3; done'

export EDITOR='emacs -nw --no-desktop'
export DIFF='ediff -nw --no-desktop'

export PATH=/usr/local/cuda-8.0/bin${PATH:+:${PATH}}

# usage: downscale_videos_in_directory input_dir output_dir
downscale_videos_in_directory() {
    input_dir=$1
    output_dir=$2
    find $input_dir -type d -exec mkdir -p $output_dir/{} \;
    find $input_dir -type f -exec ffmpeg -i {} -vf scale="640:-1" -vcodec libx264 -acodec copy $output_dir/{} \;
}

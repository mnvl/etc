
HISTFILE=$HOME/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
setopt SHARE_HISTORY

bindkey -e

setopt interactivecomments

zstyle :compinstall filename '/home/manvel/.zshrc'
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

export EDITOR='emacs -nw --no-desktop'
export DIFF='ediff -nw --no-desktop'

export JAVA_HOME="$HOME/build/jdk1.8.0_101"
export PATH="$JAVA_HOME/bin/:$PATH"

export CUDA_HOME=/usr/local/cuda
export LD_LIBRARY_PATH=${CUDA_HOME}/lib64:${LD_LIBRARY_PATH}
export PATH="$CUDA_HOME/bin:$PATH"


export PYTHONPATH="$HOME/build/caffe/build/install/python:$PYTHONPATH"

export NDK_ROOT=/home/mel/build/android-ndk-r12b
export PATH=$NDK_ROOT:$PATH

export ANDROID_SDK_ROOT=/home/mel/build/Android
export PATH=$ANDROID_SDK_ROOT:$PATH
export PATH=$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/platform-tools:$PATH

if status is-interactive
   fish_add_path -g ~/.local/bin
   fish_add_path -g /opt/homebrew/bin/
   fish_add_path -g /usr/local/bin:$PATH
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /home/sch/miniconda3/bin/conda
    eval /home/sch/miniconda3/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/home/sch/miniconda3/etc/fish/conf.d/conda.fish"
        . "/home/sch/miniconda3/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/home/sch/miniconda3/bin" $PATH
    end
end
# <<< conda initialize <<<


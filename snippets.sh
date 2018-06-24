
alias uri_escape='perl -MURI::Escape -e "while(<STDIN>) { print uri_escape(\$_) };"'
alias uri_unescape='perl -MURI::Escape -e "while(<STDIN>) { print uri_unescape(\$_) };"'
alias gplines='feedgnuplot --lines'

mp3fy_audios_in_directory() {
    if [ "$#" -ne 2 ]; then
        echo "usage: $0 input_dir output_dir" >/dev/stderr
        return 1
    fi

    input_dir=$1
    output_dir=$2

    mkdir -p $output_dir

    find $input_dir -type d -exec mkdir -p $output_dir/{} \;
    find $input_dir -type f -name "*.flac" -exec sh -c 'flac -cd "{}" | lame -b 320 -q 0 - "'"$output_dir"'/{}.mp3";' \;
}

# sudo apt-get install cuetools shntool flac
split_flac_with_cue() {
    if [ "$#" -ne 2 ]; then
        echo "usage: $0 flac_file cue_file" >/dev/stderr
        return 1
    fi

    flac_file=$1
    cue_file=$2

    shnsplit -f $cue_file -t "%n %t" -o flac $flac_file && rm $cue_file && rm $flac_file
}

downscale_videos_in_directory() {
    if [ "$#" -ne 2 ]; then
        echo "usage: $0 input_dir output_dir" >/dev/stderr
        return 1
    fi

    input_dir=$1
    output_dir=$2

    find $input_dir -type d -exec mkdir -p $output_dir/{} \;
    find $input_dir -type f -exec ffmpeg -n -i {} -f matroska -vc libx265 -vf scale="640:-1" -preset slower -crf 38 -acodec copy $output_dir/{}.mkv \;

    find $input_dir -type f -exec mediainfo --Inform="General;%Duration%" "{}" \; 2>/dev/null | awk '{s+=$1/1000} END {h=s/3600; s=s%3600; printf "%.2d:%.2d\n", int(h), int(s/60)}'
    find $output_dir -type f -exec mediainfo --Inform="General;%Duration%" "{}" \; 2>/dev/null | awk '{s+=$1/1000} END {h=s/3600; s=s%3600; printf "%.2d:%.2d\n", int(h), int(s/60)}'
}

extract_audios_from_videos_in_directory() {
    if [ "$#" -ne 3 ]; then
        echo "usage: $0 input_dir output_dir output_files_extension" >/dev/stderr
        return 1
    fi

    input_dir=$1
    output_dir=$2
    extension=$3

    find $input_dir -type d -exec mkdir -p $output_dir/{} \;
    find $input_dir -type f -exec ffmpeg -n -i {} -vf none -acodec copy $output_dir/{}.$extension \;
}

image_to_palette() {
    convert "$@"  -format %c  -depth 8  histogram:info:- | sort -n -k 1 -t :
}

flatten_current_directory() {
    find . -type f -exec rename 's|(?<!\.)/|_|g' -- {} \;
    find . -maxdepth 1 -mindepth 1 -type d -exec rm -R {} \;
}

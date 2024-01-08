#!/run/current-system/sw/bin/fish

notify-send "windows.fish $argv"

set eww_path "$HOME/new-eww/"
set cache_path "$HOME/.cache/eww"

if test ! -e $cache_path
    mkdir $cache_path
end

function open
    eww -c "$eww_path" open $argv[1]
    touch "$cache_path/$argv[1]"
end

function close
    eww -c "$eww_path" close $argv[1]
    rm "$cache_path/$argv[1]"
end

function toggle
    if test -e "$cache_path/$argv[1]"
        close $argv[1]
    else
        open $argv[1]
    end
end

switch $argv[1]
    case --open
        open $argv[2]
    case --close
        close $argv[2]
    case --toggle
        toggle $argv[2]
end

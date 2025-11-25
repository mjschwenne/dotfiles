#!/etc/profiles/per-user/mjs/bin/fish

set -l transitions simple fade left right top bottom wipe wave grow center any outer
set -l wallpapers (ls ~/.dotfiles/home/desktop/wallpapers/*)

if ! pgrep awww-daemon >/dev/null
    awww-daemon
    awww img -t (random choice $transitions) (random choice $wallpapers)
end

switch $argv[1]
    case change
        awww img -t (random choice $transitions) (random choice $wallpapers)
    case interval
        while true
            awww img -t (random choice $transitions) (random choice $wallpapers)
            sleep $argv[2]
        end
end

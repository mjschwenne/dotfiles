#!/run/current-system/sw/bin/fish

function music_title
    if test (playerctl status) = Playing
        echo (playerctl metadata --format '{{title}}')
    else
        echo offline
    end
end

function music_artist
    if test (playerctl status) = Playing
        echo (playerctl metadata --format '{{artist}}')
    else
        echo unknown
    end
end

function music_art
    set -f player_name (playerctl -l --no-messages)

    if test (playerctl status) != Playing
        echo "$HOME/new-eww/assets/no-music.png"
    else if test "$player_name" = spotify
        echo (playerctl metadata | grep artUrl | awk '{print $3}')
    else if test (string sub -e 7 "$player_name") = firefox
        # For some reason, librewolf still shows up as firefox for this
        set -f path "$HOME/.librewolf/firefox-mpris/"
        set -f image "$(ls $path)"
        echo $path$image
    else
        echo "$HOME/new-eww/assets/no-music.png"
    end
end

function music_status
    set -f m_status (playerctl status --no-messages)

    if test $m_status = Playing
        echo true
    else
        echo false
    end
end

function music_toggle
    playerctl play-pause
end

function music_next
    playerctl next --no-messages
end

function music_prev
    playerctl previous --no-messages
end

function music_position
    if test (playerctl status --no-messages) = Playing
        set -f length (math (playerctl metadata | grep length | awk '{print $3}') / 1000000)
        set -f position (playerctl position | awk '{printf("%d\n", $1)}')
        echo (math 100 x $position / $length)
    else
        echo 0
    end
end

switch $argv[1]
    case --title
        music_title
    case --artist
        music_artist
    case --art
        music_art
    case --status
        music_status
    case --toggle
        music_toggle
    case --next
        music_next
    case --prev
        music_prev
    case --position
        music_position
end

#!/run/current-system/sw/bin/fish

function volume
    set -f vol (math (wpctl get-volume @DEFAULT_SINK@ | string sub -s 9 -l 4) x 100)
    if test "$(wpctl get-volume @DEFAULT_SINK@ | string split -f 3 --allow-empty " ")" = "[MUTED]"
        set -f name vol-muted
    else if test $vol -ge 66
        set -f name vol-full
    else if test $vol -ge 33
        set -f name vol-mid
    else if test $vol -gt 00
        set -f name vol-low
    else
        set -f name vol-muted
    end

    echo "{\"name\":\"VOL\",\"img\":\"$name\",\"value\":$vol,\"click\":\"wpctl set-mute @DEFAULT_SINK@ toggle\",\"scroll\":\"/home/mjs/new-eww/scripts/volume_scroll.fish {}\"}"
end

function gpu
    set -f value (gpustat --no-processes --no-color | string match -r "\d+ %" | string split -f 1 " ")
    echo "{\"name\":\"GPU\",\"img\":\"gpu\",\"value\":$value}"
end

function battery

end

if test $hostname = terra
    echo "[$(volume),$(gpu)]"
else
    echo "[$(volume),$(battery)]"
end

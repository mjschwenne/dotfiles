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

    echo "{\"name\":\"VOL\",\"img\":\"$name\",\"value\":$vol,\"click\":\"wpctl set-mute @DEFAULT_SINK@ toggle\",\"scroll\":\"/home/mjs/.config/eww/scripts/volume_scroll.fish {}\"}"
end

function gpu
    set -f value (gpustat --no-processes --no-color | string match -r "\d+ %" | string split -f 1 " ")
    echo "{\"name\":\"GPU\",\"img\":\"gpu\",\"value\":$value}"
end

function battery
    if test $hostname = mars
        set bat /org/freedesktop/UPower/devices/battery_BAT0
    else
        set bat /org/freedesktop/UPower/devices/battery_BAT1
    end

    if test $hostname = mars
        set ac /org/freedesktop/UPower/devices/line_power_AC
    else
        set ac /org/freedesktop/UPower/devices/line_power_ADP1
    end

    set -l value (upower -i $bat | jc --upower | jq -r '.[0].detail.percentage')
    set -l plugged_in (upower -i $ac | jc --upower | jq -r '.[0].detail.online')

    set -l icons battery-empty battery-10 battery-20 battery-30 battery-40 battery-50 battery-60 battery-70 battery-80 battery-90 battery-full battery-charging-empty battery-charging-10 battery-charging-20 battery-charging-30 battery-charging-40 battery-charging-50 battery-charging-60 battery-charging-70 battery-charging-80 battery-charging-90 battery-charging-full
    set -l idx (math "ceil(($value + 1) / 10)")
    if test $plugged_in = true
        set idx (math "$idx+11")
    end

    echo "{\"name\":\"BAT\",\"img\":\"$icons[$idx]\",\"value\":$value}"
end

if test $hostname = terra
    echo "[$(volume),$(gpu)]"
else
    echo "[$(volume),$(battery)]"
end

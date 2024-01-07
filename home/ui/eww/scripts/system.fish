#!/run/current-system/sw/bin/fish

# format, JSON list structured as {"name":"<path-to-icon>", "value":val, "tooltip":"", size:val}

if test $hostname = luna
    set size 50
else
    set size 20
end

function volume
    set -f vol (math (wpctl get-volume @DEFAULT_SINK@ | string sub -s 9 -l 4) x 100)
    if test $vol -ge 66
        set -f name vol-full
    else if test $vol -ge 33
        set -f name vol-mid
    else if test $vol -gt 00
        set -f name vol-low
    else
        set -f name vol-muted
    end

    echo "{\"name\":\"$name\",\"value\":$vol,\"tooltip\":\"VOL: $vol%\",\"size\":$size}"
end

function gpu
    set -f value (gpustat --no-processes --no-color | string match -r "\d+ %" | string split -f 1 " ")
    echo "{\"name\":\"gpu\",\"value\":$value,\"tooltip\":\"GPU: $value%\",\"size\":$size}"
end

function battery

end

if test $hostname = terra
    echo "[$(volume),$(gpu)]"
else
    echo "[$(volume),$(battery)]"
end

#!/etc/profiles/per-user/mjs/bin/fish

if test $hostname = terra
    set net_ad wlo1
else if test $hostname = mars
    set net_ad wlp3s0
else if test $hostname = luna
    set net_ad wlp1s0
end

function get
    iwconfig $net_ad | jc --iwconfig | jq -c '.[0] | {name: .essid, bit_rate: .bit_rate, bit_rate_unit: .bit_rate_unit, link_quality: .link_quality}'
end

function get_icon
    set -f quality (math (echo $(get) | jq -r '.link_quality'))
    if test $quality -ge 0.8
        echo wifi-full
    else if test $quality -ge 0.6
        echo wifi-3quarters
    else if test $quality -ge 0.4
        echo wifi-half
    else if test $quality -ge 0.2
        echo wifi-1quarter
    else
        echo wifi-zero
    end
end

echo $(get) | jq -c "{name: .name, bit_rate: .bit_rate, bit_rate_unit: .bit_rate_unit, link_quality: .link_quality, icon: \"$(get_icon)\"}"
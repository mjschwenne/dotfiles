#! /etc/profiles/per-user/mjs/bin/fish

function reply -a msg
    echo $msg >/tmp/serverout.fifo
end

function stop --on-signal SIGINT
    rm /tmp/serverin.fifo
    rm /tmp/serverout.fifo
end

function battery
    set -l bat_json (
		upower -i /org/freedesktop/UPower/devices/battery_BAT0 |
		jc --upower
	)
    set -l percentage (echo $bat_json | jq -r '.[0].detail.percentage')
    set -l plugged_in (
		upower -i /org/freedesktop/UPower/devices/line_power_AC |
		jc --upower |
		jq -r '.[0].detail.online'
	)

    # set icon and class
    set -l icons 󰂎 󰁺 󰁻 󰁼 󰁽 󰁾 󰁿 󰂀 󰂁 󰂂 󰁹 󰢟 󰢜 󰂆 󰂇 󰂈 󰢝 󰂉 󰢞 󰂊 󰂋 󰂅
    set -l idx (math "ceil(($percentage + 1) / 10)")
    if test $plugged_in = true
        set idx (math "$idx+11")
    end

    if test $idx -ge 20
        set class battery-charging-full
    else if test $idx -ge 15
        set class battery-charging-normal
    else if test $idx -ge 12
        set class battery-charging-low
    else if test $idx -ge 9
        set class battery-full
    else if test $idx -ge 4
        set class battery-normal
    else
        set class battery-low
    end

    reply "{\"percentage\":$percentage,\"icon\":\"$icons[$idx]\",\"class\":\"$class\"}"
end

function brightness
    argparse -x 'q,s' q/query 's/set=?' -- $argv

    set -l icon i
    brightnessctl i intel_backlight | string match -q -r "\((?<current>[0-9]+)%\)"

    if set -q _flag_s
        brightnessctl s $_flag_s% >/dev/null
        set current $_flag_s
    end

    # Set icon and class
    set -l icons 󰛩 󱩎 󱩏 󱩐 󱩑 󱩒 󱩓 󱩔 󱩕 󱩖 󰛨
    set -l idx (math "ceil(($current + 1) / 10)")

    reply "{\"icon\":\"$icons[$idx]\",\"percentage\":$current}"
end

mkfifo /tmp/serverout.fifo
mkfifo /tmp/serverin.fifo
while read -t -a args </tmp/serverin.fifo
    switch $args[1]
        case battery
            echo calling battery $args[2..]
            battery $args[2..]
        case brightness
            echo calling brightness $args[2..]
            brightness $args[2..]
        case exit
            break
    end
end

reply ok
stop

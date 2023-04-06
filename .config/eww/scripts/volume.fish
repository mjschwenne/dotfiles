#!/bin/env fish 

function format_volume
	set fmt "(circular-metric :icon \"%s\" :value %d :tooltip \"%s\" :css-class \"%s\" :cmd \"%s\")\n"
	string match -rq "Right: .* \[(?<vol>[0-9]+)\%\] \[(?<mute>on|off)\]" (amixer sget Master)
	if test $mute = "off"
		set icon "ﱝ"
		set tooltip (printf "VOL: MUTED (%d%%)" $vol)
		set css "volume-muted"
	else 
		set icon "󰕾"
		set tooltip (printf "VOL: %d%%" $vol)
		set css "volume-unmuted"
	end
	printf $fmt $icon $vol $tooltip $css "amixer -D pulse set Master 1+ toggle"
end

format_volume
stdbuf -oL amixer sevents Master | while read __
	format_volume
end

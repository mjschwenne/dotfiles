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
set count 0
stdbuf -oL amixer sevents Master | while read _
	if test $count -gt 2
		format_volume
		set count 0
	else 
		set count (math $count + 1)
	end
end

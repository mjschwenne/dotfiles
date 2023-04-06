#!/bin/env fish 

# Code inspired by by https://github.com/elkowar/eww/issues/178

# Laptop monitor - eDP-1
set -l monitor_laptop "0x00200002"
set -l bars_laptop "eww-panel" "Polybar"

# External monitor - HDMI-1
set -l monitor_external "0x00200004"
set -l bars_external "eww-secondary-panel"

bspc subscribe node_state | while read -d " " __ mon __ __ state flag 
	if test $mon = $monitor_laptop && test $state = "fullscreen"
		for bar in $bars_laptop 
			# echo Hiding $bar
			xdotool search --class "$bar" windowunmap
		end
	else if test $mon = $monitor_laptop && test $state != "fullscreen"
		for bar in $bars_laptop 
			# echo Showing $bar
			xdotool search --class "$bar" windowmap 
		end
	else if test $mon != $monitor_laptop && test $state = "fullscreen"
		for bar in $bars_external 
			# echo Hiding $bar
			xdotool search --class "$bar" windowunmap 
		end 
	else if test $mon != $monitor_laptop && test $state != "fullscreen"
		for bar in $bars_external 
			# echo Showing $bar
			xdotool search --class "$bar" windowmap 
		end 
	end
end

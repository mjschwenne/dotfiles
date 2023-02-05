#!/bin/env fish 

set INTERNAL_MONITOR "eDP-1"
set EXTERNAL_MONITOR "HDMI-1"

function monitor_add
	# Move first five unfocused desktops to external monitor 
	set count 0
	for desktop in (string split ":" (bspc subscribe report -c 1))
		echo $desktop
		if contains (string sub -l 1 $desktop) "o" "f" "u"
			echo moving $desktop
			bspc desktop (string sub -s 2 $desktop) -m $EXTERNAL_MONITOR 
			set count (math $count + 1)
			if test $count -ge 5
				echo Breaking loop
				break
			end
		end
	end 

	# remove the default config 
	bspc desktop Desktop -r
end 

function monitor_remove 
	# Add a temp desktop to the external monitor since a minimum of one is required 
	bspc monitor $EXTERNAL_MONITOR -a Desktop 

	# Move all desktops expect the default to the internal monitor 
	for desktop in (bspc query -D -m $EXTERNAL_MONITOR)
		bspc desktop $desktop -m $INTERNAL_MONITOR 
	end 
 	
	bspc desktop Desktop -r 
	bspc monitor $EXTERNAL_MONITOR -r
	bspc monitor $INTERNAL_MONITOR -o 1 2 3 4 5 6 7 8 9 10
end

if xrandr -q | grep -q "$EXTERNAL_MONITOR connected"
	if test (bspc query -D -m $EXTERNAL_MONITOR | wc -l) -ne 5
		echo "Adding monitor"
		monitor_add 
		eww open secondary-panel
	end 
else 
	if test (bspc query -D -m $EXTERNAL_MONITOR | wc -l) -ne 10
		echo "Removing monitor"
		monitor_remove
		eww close secondary-panel
	end 
end 


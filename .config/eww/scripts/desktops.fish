#!/bin/env fish 

set desktop_box_eww '(box :class "desktops" :orientation "h" :space-evenly false :spacing 5 :width 300 :valign "center" '
set desktop_eww '(button :class "%s" :onclick "~/.config/bspwm/scripts/select-desktop.fish %d" :width 25 "%s")'

bspc subscribe report | while read desktops
	# Remove the first character, which is a W
	set desktops (string sub -s 2 $desktops | string split :)
	set output $desktop_box_eww
	for d in $desktops 
		set d_name (string sub -s 2 $d)
		set d_status (string sub -l 1 $d)
		switch $d_status
		case M
			set focused_monitor
		case m
			set -e focused_monitor
		case o f u
			# A bit of a roundabout way of testing if the desktop is an integer
			if test $d_name -gt 0
				set desktop[$d_name] $d_status
			end
		case O F U 
			if set -q focused_monitor; and test $d_name -gt 0
				set desktop[$d_name] F
			else if test $d_name -gt 0
				set desktop[$d_name] A
			end
		end
	end

	for num in (seq 10)
		switch $desktop[$num]
			case o 
				set -a output (printf $desktop_eww desktop-occupied $num $num)
			case f 
				set -a output (printf $desktop_eww desktop-free $num )
			case u
				set -a output (printf $desktop_eww desktop-urgent $num $num)
			case F 
				set -a output (printf $desktop_eww desktop-focused $num $num)
			case A 
				set -a output (printf $desktop_eww desktop-active $num $num)
		end
	end
	echo (string join " " $output)')'
end

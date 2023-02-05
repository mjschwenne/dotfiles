#! /bin/env fish 

set -l current_desktop (bspc query -D -d --names)
set -l monitor_desktops (bspc query -D -m --names)
set -l selected_desktop $argv[1]
set -l active_desktop (bspc query -D -d any.active --names)

# There are three cases here:
# 1. The requested desktop is on the currect montior 
# 2. The requested desktop is on the other monitor but not active 
# 3. The requested desktop is the active desktop on the other montior

if contains $selected_desktop $monitor_desktops 
	# Desktop on current montior: just switch to the other montior
	bspc desktop $selected_desktop -f
else if test $selected_desktop = $active_desktop 
	# Desktop active on other montior: swap desktop locations
	bspc desktop $current_desktop -s $selected_desktop
else 
	# Desktop inactive on other montior: pull to this one
	bspc desktop $selected_desktop -m focused --follow
	bspc desktop -f $selected_desktop
end

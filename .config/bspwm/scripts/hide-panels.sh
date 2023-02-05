#!/bin/env bash 

# Code comes from https://github.com/elkowar/eww/issues/178

declare -A monitors
monitors[0x00200002]="eDP-1"
monitors[0x00200004]="HDMI-1"

declare -A bars
bars["eDP-1"]="panel"
bars["HDMI-1"]="secondary-panel"

bspc subscribe node_state | while read -r _ mon _ _ state flag; do
  if [[ "$state" != fullscreen ]]; then continue; fi
  if [[ "$flag" == on ]]; then
    xdotool search --class ${bars[${monitors[$mon]}]} windowunmap
	if [[ "${monitors[$mon]}" == "eDP-1" ]]; then 
		xdotool search --class "Polybar" windowunmap
	fi
  else
    xdotool search --class ${bars[${monitors[$mon]}]} windowmap
	if [[ "${monitors[$mon]}" == "eDP-1" ]]; then 
		xdotool search --class "Polybar" windowmap
	fi
  fi
done

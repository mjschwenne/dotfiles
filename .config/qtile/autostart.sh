#!/bin/sh
picom &
xrandr --output HDMI-1 --left-of eDP-1
syncthing serve --no-browser &

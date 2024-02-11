#!/run/current-system/sw/bin/fish

if pgrep -x swayidle
    notify-send "Killed idle daemon"
    pkill swayidle
else
    notify-send "Starting idle daemon"
    swayidle -w timeout 300 swaylock timeout 600 'systemctl suspend' before-sleep swaylock &
end

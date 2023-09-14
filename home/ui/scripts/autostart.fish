#!/etc/profiles/per-user/mjs/bin/fish

function run -a program
	if ! pgrep $program > /dev/null
		echo Starting $program
		$argv &
	end 
end

# autostart programs when hyprland is launched, assuming that they are not already running

/home/mjs/.config/hypr/scripts/wallpaper.fish interval 300 &
/home/mjs/.config/hypr/scripts/monitor.fish &
eww daemon; eww open primary_panel; eww open secondary_panel
swaync &
nm-applet & 
protonmail-bridge --noninteractive &
# run syncthing --no-browser &

#!/etc/profiles/per-user/mjs/bin/fish

socat - "UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" | while read -l line
	switch (string split -f 1 -m 1 ">>" $line)
		case monitoradded
			sleep 1
			eww open secondary_panel
		case montiorremoved
			eww close secondary_panel 
	end
end

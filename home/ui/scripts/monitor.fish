#!/etc/profiles/per-user/mjs/bin/fish

socat - "UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" | while read -l line
    switch (string split -f 1 -m 1 ">>" $line)
        case monitoradded
            sleep 1
            eww open-many background_window_sec start_window_sec workspaces_window_sec window_window_sec music_window_sec tray_window_sec sys_window_sec clock_window_sec power_window_sec
        case montiorremoved
            eww close background_window_sec start_window_sec workspaces_window_sec window_window_sec music_window_sec tray_window_sec sys_window_sec clock_window_sec power_window_sec
    end
end

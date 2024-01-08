#!/etc/profiles/per-user/mjs/bin/fish

function run -a program
    if ! pgrep $program >/dev/null
        echo Starting $program
        $argv &
    end
end

# autostart programs when hyprland is launched, assuming that they are not already running

/home/mjs/.config/hypr/scripts/wallpaper.fish interval 300 &
/home/mjs/.config/hypr/scripts/monitor.fish &
eww daemon
eww open-many background_window start_window workspaces_window window_window music_window tray_window sys_window clock_window power_window
eww open-many background_window_sec start_window_sec workspaces_window_sec window_window_sec music_window_sec tray_window_sec sys_window_sec clock_window_sec power_window_sec
swaync &
nm-applet &
protonmail-bridge --noninteractive &
# run syncthing --no-browser &

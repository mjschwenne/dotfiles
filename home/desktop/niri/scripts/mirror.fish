#!/etc/profiles/per-user/mjs/bin/fish

function get_display_names --description "Extract display connection names from 'niri msg outputs'"
    string match -a -r 'Output.*\(([^)]+)\)' -g "$(niri msg outputs)"
end

function get_external_display_name --description "Return the first display name that isn't eDP-1"
    string match -v -r '^eDP-1$' (get_display_names) | head -n 1
end

# Example usage:
wl-mirror "eDP-1" 2> /dev/null &
sleep 0.1
set wl_id (string match -r 'Window ID ([0-9]+).*\\n.*\\n  App ID: \"at.yrlf.wl_mirror\"' -a -g "$(niri msg windows)")
niri msg action move-window-to-monitor --id $wl_id (get_external_display_name)
niri msg action fullscreen-window --id $wl_id
niri msg action focus-monitor eDP-1


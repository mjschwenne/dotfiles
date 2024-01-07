#!/etc/profiles/per-user/mjs/bin/fish

function window_class
    hyprctl activewindow -j | jq -r .class
end

function window_title
    hyprctl activewindow -j | jq -r .title
end

function workspaces
    set -f NUM_WORKSPACES 10
    set -f workspace_windows (hyprctl workspaces -j | jq -c 'map({key: .id | tostring, value: .windows}) | from_entries')
    set -f workspace_display (hyprctl monitors -j | jq -c '{primary: map_values(if .focused then .activeWorkspace.id | tostring else null end) | del(..|nulls), secondary: map_values(if .focused == false then .activeWorkspace.id | tostring else null end) | del(..|nulls)}')
    seq 1 $NUM_WORKSPACES | jq --argjson windows $workspace_windows --argjson monitors $workspace_display --slurp -Mc 'map(tostring) | map({id: ., windows: ($windows[.]//0), status: (if any($monitors.primary[] == .; .) then "focused" elif any($monitors.secondary[] == .; .) then "displayed" else "hidden" end)})'
end

function monitors
    hyprctl monitors -j | jq -c 'map({key: .name | tostring, value: {focused: .focused, workspace: .activeWorkspace.id}}) | from_entries'
end

switch $argv[1]
    case window
        window_class
        socat -u UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock - | while read line
            window_class
        end

    case workspaces
        workspaces
        socat -u UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock - | while read line
            set -l action (string split -m 1 -f 1 ">>" $line)
            if test $action = focusedmon -o $action = workspace
                workspaces
            end
        end

    case test
        workspaces
end

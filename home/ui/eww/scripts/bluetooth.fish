#!/etc/profiles/per-user/mjs/bin/fish

function get_bluetooth_state
    set -l state (hcitool dev | wc -l)
    set -l icon bluetooth-on
    if test $state -gt 1
        set state on
    else
        set state off
        set icon bluetooth-off
    end

    set -l current_devices (bluetoothctl devices | string match -r -a "(?:[0-9A-F]{2}:){5}[0-9A-F]{2}")
    for device in $current_devices
        set -f -a names "$(bluetoothctl info $device | jc --bluetoothctl |
			jq -c -r 'map_values(if .connected == "yes" then .name else "" end) |.[0]')"
    end

    set current_devices (string join -n "|" $names | jq -R -c '. |= split("|")')
    if test -z $current_devices
        set current_devices "[]"
    end
    echo "{\"state\":\"$state\",\"icon\":\"$icon\",\"devices\":$current_devices}"
end

get_bluetooth_state
dbus-monitor --system --profile "sender='org.bluez'" 2>/dev/null | while read __
    get_bluetooth_state
end
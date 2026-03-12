{ pkgs }:
pkgs.writeShellApplication {
  name = "brightness-notify";
  runtimeInputs = [
    pkgs.brightnessctl
    pkgs.libnotify
  ];
  text = ''
    # Change brightness, then show a notification.
    # Usage: brightness-notify --class=backlight set +10%
    #        brightness-notify --class=backlight set 10%-
    brightnessctl "$@"

    # Get current brightness percentage
    pct=$(brightnessctl --class=backlight info | grep -oP '\d+(?=%)')

    # Build a progress bar
    bar_length=17
    filled=$(( pct * bar_length / 100 ))
    empty=$(( bar_length - filled ))
    bar=$(printf '%0.s█' $(seq 1 "$filled" 2>/dev/null) ; printf '%0.s░' $(seq 1 "$empty" 2>/dev/null))

    if (( pct == 0 )); then
        icon="display-brightness-off"
    elif (( pct <= 33 )); then
        icon="display-brightness-low"
    elif (( pct <= 66 )); then
        icon="display-brightness-medium"
    else
        icon="display-brightness-high"
    fi
    summary="Brightness: $pct%"

    notify-send \
        --app-name "brightness" \
        --hint "string:x-canonical-private-synchronous:brightness" \
        --icon "$icon" \
        "$summary" "$bar"
  '';
}

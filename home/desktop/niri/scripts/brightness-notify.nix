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
        --hint "int:value:$pct" \
        "$summary"
  '';
}

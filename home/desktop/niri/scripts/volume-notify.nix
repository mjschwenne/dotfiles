{ pkgs }:
pkgs.writeShellApplication {
  name = "volume-notify";
  runtimeInputs = [
    pkgs.wireplumber
    pkgs.libnotify
  ];
  text = ''
    # Change volume/mute, then show a notification.
    # Usage: volume-notify set-volume @DEFAULT_AUDIO_SINK@ 0.1+ -l 1.0
    #        volume-notify set-mute @DEFAULT_AUDIO_SINK@ toggle
    wpctl "$@"

    volume=$(wpctl get-volume @DEFAULT_AUDIO_SINK@)

    # Check mute status
    if [[ "$volume" == *"[MUTED]"* ]]; then
        muted=true
    else
        muted=false
    fi

    # Extract the decimal volume (e.g., "0.50") and convert to percentage
    pct=$(echo "$volume" | awk '{printf "%.0f", $2 * 100}')

    if $muted; then
        icon="notification-audio-volume-muted"
        summary="Volume: Muted ($pct%)"
    else
        if (( pct == 0 )); then
            icon="notification-audio-volume-muted"
        elif (( pct <= 33 )); then
            icon="notification-audio-volume-low"
        elif (( pct <= 66 )); then
            icon="notification-audio-volume-medium"
        else
            icon="notification-audio-volume-high"
        fi
        summary="Volume: $pct%"
    fi

    # add `--hint "int:value:$pct" \` to shade the progress whole notification
    notify-send \
        --app-name "volume" \
        --hint "string:x-canonical-private-synchronous:volume" \
        --icon "$icon" \
        --hint "int:value:$pct" \
        "$summary"
  '';
}

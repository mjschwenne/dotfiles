{ pkgs }:
pkgs.writeShellApplication {
  name = "tailscale-status";
  runtimeInputs = [
    pkgs.tailscale
    pkgs.jq
    pkgs.libnotify
  ];
  text = ''
    status=$(tailscale status --json)
    exit_hostname=$(echo "$status" | jq -r '.Peer[] | select(.ExitNode == true) | .HostName')

    if [ -z "$exit_hostname" ]; then
      notify-send \
        --app-name "tailscale" \
        --hint "string:x-canonical-private-synchronous:tailscale" \
        --icon "network-vpn-disconnected" \
        "VPN: No exit node active"
    elif echo "$exit_hostname" | grep -qi mullvad; then
      notify-send \
        --app-name "tailscale" \
        --hint "string:x-canonical-private-synchronous:tailscale" \
        --icon "network-vpn" \
        "VPN: Mullvad active" "$exit_hostname"
    else
      notify-send \
        --app-name "tailscale" \
        --hint "string:x-canonical-private-synchronous:tailscale" \
        --icon "network-vpn" \
        "VPN: Exit node active" "$exit_hostname"
    fi
  '';
}

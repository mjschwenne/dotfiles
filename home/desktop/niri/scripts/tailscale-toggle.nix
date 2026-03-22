{ pkgs, exitNode }:
pkgs.writeShellApplication {
  name = "tailscale-exit-toggle";
  runtimeInputs = [
    pkgs.tailscale
    pkgs.jq
    pkgs.libnotify
  ];
  text = ''
    active=$(tailscale status --json | jq -r '.Peer[] | select(.ExitNode == true) | .HostName')

    if [ -z "$active" ]; then
      tailscale set --exit-node="${exitNode}"
      notify-send \
        --app-name "tailscale" \
        --hint "string:x-canonical-private-synchronous:tailscale" \
        --icon "network-vpn" \
        "VPN: Mullvad enabled" "${exitNode}"
    else
      tailscale set --exit-node=
      notify-send \
        --app-name "tailscale" \
        --hint "string:x-canonical-private-synchronous:tailscale" \
        --icon "network-vpn-disconnected" \
        "VPN: Exit node disabled"
    fi
  '';
}
